module Cryptol.Compiler.Cry2IR.ConvertM where

import Data.Map(Map)
import Data.Map qualified as Map
import MonadLib

import Cryptol.ModuleSystem.Name qualified as Cry
import Cryptol.TypeCheck.Type qualified as Cry

import Cryptol.Compiler.PP
import Cryptol.Compiler.Error(panic,Loc)
import Cryptol.Compiler.Monad qualified as M
import Cryptol.Compiler.IR.Cryptol


-- | Monad used to translate Cryptol expressions to IR
newtype ConvertM a = ConvertM (SpecImpl a)
  deriving (Functor,Applicative,Monad) via SpecImpl

-- | This is the implementation of the monad
type SpecImpl =
  WithBase M.CryC
    '[ ReaderT RO
     ]

data RO = RO
  { roNumericParams :: Map Cry.TParam StreamSize
    -- ^ Information about type parameters.
    -- These will be either the variable used to represent them, or
    -- a constant, if we are working on an instantiation where this
    -- parameter is fixed.

  , roLocalIRNames  :: Map NameId Name
    -- ^ Maps Cryptol name to an IR name, which has the IRType of the local
  }


runConvertM :: ConvertM a -> M.CryC a
runConvertM (ConvertM m) = runReaderT ro m
  where
  ro = RO
    { roNumericParams = mempty
    , roLocalIRNames = mempty
    }



-- | Do some IO.
doIO :: IO a -> ConvertM a
doIO m = doCryC (M.doIO m)

-- | Do CryC stuff.
doCryC :: M.CryC a -> ConvertM a
doCryC m = ConvertM (inBase m)

-- | Do some nested CryC stuff.
doCryCWith :: (forall a. M.CryC a -> M.CryC a) -> ConvertM b -> ConvertM b
doCryCWith k (ConvertM m) =
  do ro <- ConvertM ask
     doCryC (k (runReaderT ro m))

-- | Get the value of a numeric type parameter.
lookupNumericTParam :: Cry.TParam -> ConvertM StreamSize
lookupNumericTParam tp =
  do ro <- ConvertM ask
     case Map.lookup tp (roNumericParams ro) of
       Just s  -> pure s
       Nothing -> panic "lookupNumericTParam"
                    [ "Missing numeric parameter", show (pp tp) ]


--------------------------------------------------------------------------------

-- | Abort: we found something that's unsupported.
unsupported :: Doc -> ConvertM a
unsupported x = doCryC (M.unsupported ("[cry2ir]" <+> x))

enterLoc :: Loc -> ConvertM a -> ConvertM a
enterLoc loc = doCryCWith (M.enterLoc loc)

enterFun :: Cry.Name -> FunInstance -> ConvertM a -> ConvertM a
enterFun f i = enterLoc [ cryPP f <+> pp i ]

enterLocal :: Cry.Name -> ConvertM a -> ConvertM a
enterLocal cnm = enterLoc [ cryPP cnm ]

--------------------------------------------------------------------------------

withNumericTParams :: [(Cry.TParam, StreamSize)] -> ConvertM a -> ConvertM a
withNumericTParams xs (ConvertM m) = ConvertM (mapReader upd m)
  where
  upd ro = ro { roNumericParams = foldr (uncurry Map.insert)
                                        (roNumericParams ro) xs }

withLocals :: [(Name, Cry.Type)] -> ConvertM a -> ConvertM a
withLocals xs k =
  doCryCWith (M.withCryLocals [ (x,t) | (IRName (NameId x) _, t) <- xs ]) $
  withIRLocals (map fst xs) k

-- | Add some locals for the duration of a compiler computation
withIRLocals :: [Name] -> ConvertM a -> ConvertM a
withIRLocals locs (ConvertM m) = ConvertM (mapReader upd m)
  where
  upd ro = ro { roLocalIRNames = Map.union locNs (roLocalIRNames ro) }
  locNs  = Map.fromList [ (x,n) | n@(IRName x _) <- locs ]

getLocal :: NameId -> ConvertM (Maybe Name)
getLocal x = Map.lookup x . roLocalIRNames <$> ConvertM ask





