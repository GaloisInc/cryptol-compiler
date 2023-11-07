module Cryptol.Compiler.Rust.Monad where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
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
  { extModuleName  :: RustPath
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
  do i <- bindLocal (addLocalVar True) x
     a <- k i
     Rust (sets_ \rw -> rw { rwLocalNames =
                               removeLocalVar x i (rwLocalNames rw)})
     pure a

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
  where
  getUsed   = lUsedVars . rwLocalNames <$> Rust get
  setUsed x = Rust $ sets_ \rw -> let ls = rwLocalNames rw
                                  in rw { rwLocalNames = ls { lUsedVars = x } }

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
lookupSizeParam :: Cry.TParam -> Rust RustExpr
lookupSizeParam x =
  Rust (pathExpr . simplePath . lookupName x . lTypeNames . rwLocalNames <$> get)

-- | Get the identfier for a name.
-- Returns (isThisLocal?, isThisLastUse?,Expr)
-- isThisLocal is True if this is `let` bound local variable
-- isThisLastUse is True if the variable is not mentioned in the continuation.
lookupNameId :: NameId -> Rust (Bool,Bool,RustExpr)
lookupNameId x =
  do rw <- Rust get
     let locals     = rwLocalNames rw
     let rid        = lookupName x (lValNames (rwLocalNames rw))
     let isLoc      = rid `Set.member` lLocalVars locals
     let used       = lUsedVars locals
     let isLastUse  = not (rid `Set.member` used)
     Rust $ set $ rw { rwLocalNames = locals { lUsedVars = Set.insert rid used }
                     }
     pure (isLoc, isLastUse, pathExpr (simplePath rid))


-- | Get the expression for a length parametes associated with the given
-- type parameter.
lookupLenParam :: Cry.TParam -> Rust RustExpr
lookupLenParam x =
  Rust (pathExpr . simplePath . doLookup . lLenParams . rwLocalNames <$> get)
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
      do -- let mo = nameIdModule f
         ro <- Rust ask
         let mo = roModName ro -- XXX: For now everything goes in one module
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
                  let Rust.Path glob segs _ = extModuleName ext
                  i <- case Map.lookup fu (extModuleNames ext) of
                         Just it -> pure it
                         Nothing -> panic "lookupFunName"
                                      [ "Missing function"
                                      , "Module: " ++ show (cryPP mo)
                                      , "Function: " ++ show (pp fu)
                                      ]
                  let seg = Rust.PathSegment i Nothing ()
                  -- XXX: Maybe we should record that we used this module
                  -- so we can add imports, or maybe they are not needed?
                  pure (Rust.Path glob (segs ++ [seg]) ())

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
  { lTypeNames  :: NameMap Cry.TParam           -- ^ Names for type params
  , lValNames   :: NameMap NameId               -- ^ Names for local values
  , lLenParams  :: Map Cry.TParam Rust.Ident
    -- ^ Names for Length params.  When generating these we use the "used"
    -- set of `lValNames`, because they are values.

  , lLocalVars     :: Set Rust.Ident
    -- ^ Names of local variable (i.e., not arguments)

  , lUsedVars      :: Set Rust.Ident
    {- ^ Variables used by the current continuation.
       We generate code "backwards" (continuation first),
       and this field tells us which variables were mentioned in the
       continuation. -}
  }

-- | Empty local names, useful when starting a new declaration.
emptyLocalNames :: LocalNames
emptyLocalNames = LocalNames
  { lTypeNames  = emptyNameMap
  , lValNames   = emptyNameMap
  , lLenParams  = mempty
  , lLocalVars  = Set.empty
  , lUsedVars   = Set.empty
  }

-- | Bind a type parameter.
addLocalType :: Cry.TParam -> LocalNames -> (Rust.Ident, LocalNames)
addLocalType t ns = (i, ns { lTypeNames = mp })
  where
  (i, mp) = addName t (lTypeNames ns)

-- | Bind a local variable/paramter.
addLocalVar :: Bool -> NameId -> LocalNames -> (Rust.Ident, LocalNames)
addLocalVar isLoc x ns = (i, ns { lValNames = mp
                                , lLocalVars =
                                      if isLoc then Set.insert i curLocs
                                               else curLocs
                                })
  where
  (i, mp) = addName x (lValNames ns)
  curLocs = lLocalVars ns

removeLocalVar :: NameId -> Rust.Ident -> LocalNames -> LocalNames
removeLocalVar x r ns =
  ns { lValNames = removeName x r (lValNames ns)
     , lLocalVars = Set.delete r (lLocalVars ns)
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

