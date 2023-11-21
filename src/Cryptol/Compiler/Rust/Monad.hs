module Cryptol.Compiler.Rust.Monad where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Maybe(isJust)
import MonadLib

import Language.Rust.Syntax qualified as Rust
import Language.Rust.Data.Ident qualified as Rust

import Cryptol.Utils.Ident qualified as Cry
import Cryptol.TypeCheck.AST qualified as Cry

import Cryptol.Compiler.PP
import Cryptol.Compiler.Error(panic,Loc)
import Cryptol.Compiler.Error qualified as Error
import Cryptol.Compiler.IR.Cryptol
import Cryptol.Compiler.Rust.Utils
import Cryptol.Compiler.Rust.Names
import Cryptol.Compiler.Rust.NameMap

data GenInfo =
  GenInfo
    { genCurModule :: Cry.ModName
    , genExternalModules :: Map Cry.ModName ExtModule
    }

runRustM :: GenInfo -> Rust a -> IO a
runRustM gi (Rust m) = fst <$> runStateT rw (runReaderT ro m)
  where
  ro =
    RO
      { roModName = genCurModule gi
      , roExternalNames = genExternalModules gi
      , roLoc = []
      }
  rw =
    RW
      { rwLocalFunNames   = emptyNameMap
      , rwLocalNames      = emptyLocalNames
      }

data ExprContext =
    OwnContext    -- ^ We are generating an ownded expression
  | BorrowContext -- ^ We are generating a borrowed expression
    deriving Eq

-- | Use Own for streams and functions, and Borrow otherwise
ownIfStream :: Type -> ExprContext
ownIfStream t =
  case t of
    TStream {} -> OwnContext
    TFun {}    -> OwnContext
    _          -> BorrowContext

type RustImpl =
  WithBase IO
    [ ReaderT RO
    , StateT  RW
    ]

newtype Rust a = Rust (RustImpl a)
  deriving (Functor,Applicative,Monad) via RustImpl

-- | Run an IO computation in the Rust monad
doIO :: IO a -> Rust a
doIO io = Rust (inBase io)

-- | Information about previously compiled modules.
data ExtModule = ExtModule
  { extModuleName  :: Rust.Ident
    -- ^ Name of module

  , extModuleNames :: Map FunName Rust.Ident
    -- ^ Functions defined in the module

  -- XXX: When we add support for `newtypes` we should have some type
  -- definitions also.
  }

data RO = RO
  { roModName :: Cry.ModName
    -- ^ The current module we are working on

    -- XXX: The segments of a mod name are cryptol identifiers
    -- so we'd need to translate those too, although more commonly
    -- these are file names so they are not likely to contain weird
    -- things such as '

  , roExternalNames :: Map Cry.ModName ExtModule
    -- ^ Names defined in different modules. Read only.

  , roLoc :: Loc
    -- ^ Location, for error reporting

  }

data RW = RW
  { rwLocalFunNames :: NameMap FunName
    -- ^ Names in the current module

  , rwLocalNames    :: LocalNames
    -- ^ Names in the current function

  }


-- | Reset names local to a function.
resetFunLocalNames :: Rust ()
resetFunLocalNames = Rust (sets_ reset)
  where
  reset rw = rw { rwLocalNames = emptyLocalNames }

-- | Bind a local names
bindLocal ::
  (a -> LocalNames -> (Rust.Ident,LocalNames)) {- ^ How to bind it -} ->
  a {- ^ Name of the local thing -} ->
  Rust Rust.Ident
bindLocal how x =
  Rust $ sets \rw -> let (i,ls) = how x (rwLocalNames rw)
                     in (i, rw { rwLocalNames = ls })


-- | Bind a local for the duraiton of the given computation
bindLocalLet :: NameId -> (Rust.Ident -> Rust a) -> Rust a
bindLocalLet x k =
  do inClo <- isInStreamClosure
     let how = case inClo of
                 Just _  -> KnownLocal
                 Nothing -> LocalOrClosure
     i <- bindLocal (addLocalVar (Just how)) x
     a <- k i
     Rust (sets_ \rw -> rw { rwLocalNames =
                               removeLocalVar x i (rwLocalNames rw)})
     pure a


-- | In some cases, instead of using `bindLocalLet` it is more convenient
-- to first add some locals and them remove them.  This avoids having
-- to nest stuff.
removeLocalLet :: NameId -> Rust ()
removeLocalLet x =
  do i <- lookupNameRustIdent x
     Rust (sets_ \rw ->
                  rw { rwLocalNames = removeLocalVar x i (rwLocalNames rw) })

-- | Add a type bound for a parameter.
addTypeBound :: Cry.TParam -> RustWherePredicate -> Rust ()
addTypeBound x b =
  Rust $ sets_ \rw ->
          let ln = rwLocalNames rw
              bs = Map.insertWith (++) x [b] (lTypeBounds ln)
          in rw { rwLocalNames = ln { lTypeBounds = bs } }

-- | Get the type bounds for this parameter.
getTypeBounds :: Cry.TParam -> Rust [RustWherePredicate]
getTypeBounds x =
  do bs <- Rust (lTypeBounds . rwLocalNames <$> get)
     pure (Map.findWithDefault [] x bs)


{- | Execute the given computation with the same continuation.
This means that all expressions will see the same set of used variables.
The resulting expressions uses a variable if any of the computation does.
(e.g., then "then" and "else" branches of if-then-else) -}
withSameCont :: [Rust a] -> Rust [a]
withSameCont ms =
  do used0 <- getUsed
     (as,us) <- flip mapAndUnzipM ms \m ->
       do a <- m
          us <- getUsed
          setUsed used0
          pure (a,us)
     setUsed (Set.unions us)
     pure as

getUsed :: Rust (Set Rust.Ident)
getUsed   = lUsedVars . rwLocalNames <$> Rust get

setUsed :: Set Rust.Ident -> Rust ()
setUsed x = Rust $ sets_ \rw -> let ls = rwLocalNames rw
                                  in rw { rwLocalNames = ls { lUsedVars = x } }

-- | Do something without affecting the continuation, and variables
-- are only accessed through Self.
inStreamClosure :: ExprContext -> Rust a -> Rust ( [(Rust.Ident,Cry.TParam)]
                                                 , [(Rust.Ident,SizeVarSize)]
                                                 , a
                                                 )
inStreamClosure ctxt (Rust m) =
  Rust
  do old <- sets $ \rw ->
                let names = rwLocalNames rw
                    start = StreamClosureInfo
                              { cloContext = ctxt
                              , cloSizeIdents = mempty
                              , cloLenIdents = mempty
                              }
                    new = names { lInStreamClosure = Just start }
                in (lInStreamClosure names, rw { rwLocalNames = new })
     a <- m
     sets \rw ->
       let names = rwLocalNames rw
       in
       case lInStreamClosure names of
         Just clo ->
           let res = ( Map.toList (cloLenIdents clo)
                     , Map.toList (cloSizeIdents clo)
                     , a)
           in (res, rw { rwLocalNames = names { lInStreamClosure = old } })
         Nothing -> panic "inStreamClosure" ["Closure infor disappeared"]

-- | Are we in a strem closure?
isInStreamClosure :: Rust (Maybe StreamClosureInfo)
isInStreamClosure = Rust (lInStreamClosure . rwLocalNames <$> get)

mapStreamClosure :: (StreamClosureInfo -> StreamClosureInfo) -> Rust Bool
mapStreamClosure f =
  Rust $ sets \rw ->
    let names = rwLocalNames rw
    in case lInStreamClosure names of
         Nothing -> (False, rw)
         Just yes -> (True, rw { rwLocalNames =
                            names { lInStreamClosure = Just (f yes) }})

-- | Bind a function in this module
bindFun :: FunName -> Rust Rust.Ident
bindFun x =
  Rust $ sets \rw -> let (i,fs) = addName x (rwLocalFunNames rw)
                    in (i, rw { rwLocalFunNames = fs })


getTParams :: Rust (Cry.TParam -> RustType)
getTParams =
  do tys <- lTypeNames . rwLocalNames <$> Rust get
     pure \x ->
       let i = lookupName x tys
           seg   = Rust.PathSegment i Nothing ()
           path  = Rust.Path False [seg] ()
       in Rust.PathTy Nothing path ()

-- | Get the identifiier corresponding to a type parameter.
lookupTParam :: Cry.TParam -> Rust Rust.Ident
lookupTParam x = Rust (lookupName x . lTypeNames . rwLocalNames <$> get)

-- | Get the expresssion for a size parameter.
lookupSizeParam :: SizeName -> Rust RustExpr
lookupSizeParam x =
  do i <- Rust (doLookup (irsName x) . lSizeParams . rwLocalNames <$> get)
     let sz = irsSize x
     yes <- mapStreamClosure \c ->
                       c { cloSizeIdents = Map.insert i sz (cloSizeIdents c) }
     pure (if yes
             then fieldSelect (pathExpr (simplePath "this")) i
             else pathExpr (simplePath i))

  where
  doLookup p mp =
    case Map.lookup p mp of
      Just a -> a
      Nothing -> panic "lookupSizeParam" ["Unknown size parameter"]



-- | Get the identfier for a name.
-- Returns (isThisLocal?, isThisLastUse?,Expr)
-- isThisLocal is True if this is `let` bound local variable
-- isThisLastUse is True if the variable is not mentioned in the continuation.
lookupNameId :: NameId -> Rust (Bool,Bool,RustExpr)
lookupNameId x =
  do rw <- Rust get
     let locals     = rwLocalNames rw
     let rid        = lookupName x (lValNames (rwLocalNames rw))
     let isLoc      = Map.lookup rid (lLocalVars locals)
     inClo <-
       do mb <- isInStreamClosure
          pure case mb of
                 Nothing -> Nothing
                 Just _ | Just KnownLocal <- isLoc -> Nothing
                 Just mode                         -> Just mode
     case inClo of
       Just mode ->
         pure ( cloContext mode == OwnContext, False
              , fieldSelect (pathExpr (simplePath "this")) rid
              )
       Nothing ->
         do let used       = lUsedVars locals
            let isLastUse  = not (rid `Set.member` used)
            Rust $ set $ rw { rwLocalNames =
                                   locals { lUsedVars = Set.insert rid used }
                            }
            pure (isJust isLoc, isLastUse, pathExpr (simplePath rid))

-- | Get the identifier for to use for the given local names
lookupNameRustIdent :: NameId -> Rust Rust.Ident
lookupNameRustIdent x =
  do rw <- Rust get
     pure (lookupName x (lValNames (rwLocalNames rw)))


-- | Get the expression for a length parametes associated with the given
-- type parameter.
lookupLenParam :: Cry.TParam -> Rust RustExpr
lookupLenParam x =
  do i <- Rust (doLookup . lLenParams . rwLocalNames <$> get)
     yes <- mapStreamClosure \c ->
                        c { cloLenIdents = Map.insert i x (cloLenIdents c) }
     pure (if yes
             then fieldSelect (pathExpr (simplePath "this")) i
             else pathExpr (simplePath i))
  where
  doLookup mp =
    case Map.lookup x mp of
      Just a  -> a
      Nothing -> panic "lookupLenParam"
                   [ "Undefined length parameter."
                   , show (pp x)
                   ]

-- | Is this name in the module that we are currently compiling.
isFunNameLocal :: FunName -> Rust Bool
isFunNameLocal fu =
  do cur <- roModName <$> Rust ask
     pure case irfnName fu of
            IRDeclaredFunName name -> nameIdModule name == cur
            _ -> False

-- | Get an expression corresponding to a named function
lookupFunName :: FunName -> Rust (Either IRPrim RustPath)
lookupFunName fu =
  case irfnName fu of
    IRPrimName p -> pure (Left p)
    IRDeclaredFunName f ->
      do let mo = nameIdModule f
         ro <- Rust ask
         rw <- Rust get
         Right <$>
           if roModName ro == mo
             then
               do let i = lookupName fu (rwLocalFunNames rw)
                      seg = Rust.PathSegment i Nothing ()
                  pure (Rust.Path False [seg] ())
             else
               do ext <- case Map.lookup mo (roExternalNames ro) of
                           Just e -> pure e
                           Nothing ->
                             panic "lookupFunName" $
                               [ "Function that failed to resolve"
                               , show (pp fu)
                               , "Missing module"
                               , show (cryPP mo)
                               , "External modules"
                               ] ++ map (show.cryPP)
                                   (Map.keys (roExternalNames ro))
                  case Map.lookup fu (extModuleNames ext) of
                    Just it -> pure (simplePath' [ extModuleName ext, it ])
                    Nothing -> panic "lookupFunName"
                                 [ "Missing function"
                                 , "Module: " ++ show (cryPP mo)
                                 , "Function: " ++ show (pp fu)
                                 ]

-- | Evaluate a computation, forgetting any locally bound variables afterward.
localScope :: Rust a -> Rust a
localScope (Rust ma) =
  Rust
  do  names <- rwLocalNames <$> get
      r <- ma
      _ <- sets_ (\s -> s { rwLocalNames = names })
      pure r

-- | Get the ampping between IR names and Rust names for this module.
getFunNames :: Rust (Map FunName Rust.Ident)
getFunNames = lMap . rwLocalFunNames <$> Rust get

--------------------------------------------------------------------------------
-- Local names

-- | Names local to a declaration
data LocalNames = LocalNames
  { lTypeNames  :: NameMap Cry.TParam   -- ^ Names for type params
  , lTypeBounds :: Map Cry.TParam [RustWherePredicate]
    -- ^ Bounds on type parameters we have.  We keep this around,
    -- so that if we need to make a stream closure, we can copy them
    -- in the struct declaration.

  , lValNames   :: NameMap NameId       -- ^ Names for local values

  , lLenParams  :: Map Cry.TParam Rust.Ident
    -- ^ Names for Length params.  When generating these we use the "used"
    -- set of `lValNames`, because they are values.

  , lSizeParams :: Map Cry.TParam Rust.Ident
    -- ^ Names for size parameters.  When generating these we use the "used"
    -- set of `lValNames`, because they are values.

  , lLocalVars     :: Map Rust.Ident LocVarLocation
    -- ^ Names of local variable (i.e., not arguments)

  , lUsedVars      :: Set Rust.Ident
    {- ^ Variables used by the current continuation.
       We generate code "backwards" (continuation first),
       and this field tells us which variables were mentioned in the
       continuation. -}

  , lInStreamClosure :: !(Maybe StreamClosureInfo)
    -- ^ If this is `Just`, then we are compiling a stream.
    --  * Then we should access local variables through self.
    --  * The ExprContext indicates how variables are stored in the closure
    --    (i.e., are they borrowed or owned by the closure).

  }

data StreamClosureInfo = StreamClosureInfo
  { cloLenIdents  :: Map Rust.Ident Cry.TParam
    -- ^ Lenght variables that need to be stored in the closure

  , cloSizeIdents :: Map Rust.Ident SizeVarSize
    -- ^ Size variables that need to be stored in the closure

  , cloContext    :: ExprContext
  }

-- | How to access a local variable
data LocVarLocation =
    KnownLocal        -- ^ Always access as a local variable
  | LocalOrClosure    -- ^ Acces through closure, if in a closure context.

-- | Empty local names, useful when starting a new declaration.
emptyLocalNames :: LocalNames
emptyLocalNames = LocalNames
  { lTypeNames  = emptyNameMap
  , lTypeBounds = mempty
  , lValNames   = emptyNameMap
  , lLenParams  = mempty
  , lSizeParams = mempty
  , lLocalVars  = mempty
  , lUsedVars   = mempty
  , lInStreamClosure = Nothing
  }

-- | Bind a type parameter.
addLocalType :: Cry.TParam -> LocalNames -> (Rust.Ident, LocalNames)
addLocalType t ns = (i, ns { lTypeNames = mp })
  where
  (i, mp) = addName t (lTypeNames ns)


-- | Bind a local variable/paramter.
addLocalVar ::
  Maybe LocVarLocation -> NameId -> LocalNames -> (Rust.Ident, LocalNames)
addLocalVar isLoc x ns = (i, ns { lValNames = mp
                                , lLocalVars =
                                     case isLoc of
                                       Nothing -> curLocs
                                       Just b  -> Map.insert i b curLocs
                                })
  where
  (i, mp) = addName x (lValNames ns)
  curLocs = lLocalVars ns

removeLocalVar :: NameId -> Rust.Ident -> LocalNames -> LocalNames
removeLocalVar x r ns =
  ns { lValNames = removeName x r (lValNames ns)
     , lLocalVars = Map.delete r (lLocalVars ns)
     , lUsedVars  = Set.delete r (lUsedVars ns)
     }


-- | Bind a local parameter used for the Length trait.
addLocalLenghtParam :: Cry.TParam -> LocalNames -> (Rust.Ident, LocalNames)
addLocalLenghtParam t ns = (i, ns { lValNames  = newVals
                                  , lLenParams = newPa
                                  })
  where
  vals    = lValNames ns
  used    = lUsed vals
  i       = rustIdentAvoiding used (rustIdent (TraitLengthName t))
  newVals = vals { lUsed = Set.insert i used }
  newPa   = Map.insert t i (lLenParams ns)


-- | Bind a local parameter used for the Length trait.
addLocalSizeParam :: Cry.TParam -> LocalNames -> (Rust.Ident, LocalNames)
addLocalSizeParam t ns = (i, ns { lValNames  = newVals
                                , lSizeParams = newPa
                                })
  where
  vals    = lValNames ns
  used    = lUsed vals
  i       = rustIdentAvoiding used (rustIdent (SizeParamName t))
  newVals = vals { lUsed = Set.insert i used }
  newPa   = Map.insert t i (lSizeParams ns)








--------------------------------------------------------------------------------
-- Errors


-- | Enter a named scope (e.g., a delcaration)
enter :: Doc -> Rust a -> Rust a
enter nm (Rust m) = Rust (mapReader (\ro -> ro { roLoc = nm : roLoc ro }) m)

-- | Raise an error, that something is not yet supported
unsupported :: Doc -> Rust a
unsupported msg =
  do l <- roLoc <$> Rust ask
     Error.unsupported (reverse l) ("[ir2rust]" <+> msg)

