-- |  Pretty prinitng support
module Cryptol.Compiler.PP
  ( Doc
  , PP(..)
  , runDoc

    -- * Configuration
  , getPPCfg
  , updPPCfg
  , withPrec
  , withTypes
  , PPCfg(..)

    -- * Combinators
  , (<+>), (<.>), hsep, hcat
  , ($$), vcat, vsep
  , commaSep, parens, parensAfter, brackets, braces
  , nest
  , showPP

    -- * Cryptol pretty printing
  , cryPP
  ) where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Text.PrettyPrint as PP
import Data.String

import Cryptol.Utils.Ident                  qualified as Cry
import Cryptol.Utils.PP                     qualified as Cry
import Cryptol.ModuleSystem.Name            qualified as Cry
import Cryptol.TypeCheck.Type               qualified as Cry

-- | Configuration for pretty printing
data PPCfg = PPCfg
  { ppPrec            :: !Int   -- ^ Precedence context
  , ppShowTypes       :: !Bool  -- ^ Should we show detailed type information
  , ppShowNameUniques :: !Bool  -- ^ Should we show name uniques
  }

-- | Default pretty printing configuration.
defaultPPConfig :: PPCfg
defaultPPConfig = PPCfg
  { ppPrec            = 0
  , ppShowTypes       = False
  , ppShowNameUniques = False
  }

-- | A configurable document.
newtype Doc = Doc (PPCfg -> PP.Doc)

instance Show Doc where
  show = show . runDoc

-- | Render a docuemtn with the default configuration.
-- Use 'updPPCfg' to provide a speicific configuration.
runDoc :: Doc -> PP.Doc
runDoc (Doc f) = f defaultPPConfig

-- | Pretty printing for compiler types.
class PP a where

  -- | Get a readable representation of a type.
  pp :: a -> Doc

-- | Get the current configuration.
getPPCfg :: (PPCfg -> Doc) -> Doc
getPPCfg f = Doc \cfg ->
  let Doc g = f cfg
  in g cfg

-- | Update the configuration for a document.
updPPCfg :: (PPCfg -> PPCfg) -> Doc -> Doc
updPPCfg f (Doc g) = Doc (g . f)

-- | Change the precedence config for a given document.
withPrec :: Int -> Doc -> Doc
withPrec n = updPPCfg \cfg -> cfg { ppPrec = n }

withTypes :: Doc -> Doc
withTypes = updPPCfg \cfg -> cfg { ppShowTypes = True }


--------------------------------------------------------------------------------
-- Lift standard combinators


lift :: PP.Doc -> Doc
lift d = Doc \_ -> d

lift1 :: (PP.Doc -> PP.Doc) -> Doc -> Doc
lift1 f (Doc a) = Doc \cfg -> f (a cfg)

lift2 :: (PP.Doc -> PP.Doc -> PP.Doc) -> Doc -> Doc -> Doc
lift2 f (Doc a) (Doc b) = Doc \cfg -> f (a cfg) (b cfg)

liftMany :: ([PP.Doc] -> PP.Doc) -> [Doc] -> Doc
liftMany f xs = Doc \cfg -> f [ g cfg | Doc g <- xs ]

-- | This is not great
instance PP Cry.Doc where
  pp d = lift (PP.text (show (Cry.runDocWith Cry.defaultPPCfg  d)))

-- | Pretty print something using Cryptol's pretty printing.
cryPP :: Cry.PP a => a -> Doc
cryPP = pp . Cry.pp

instance PP Int where
  pp x = lift (PP.int x)

instance PP Integer where
  pp x = lift (PP.integer x)

instance PP Text where
  pp x = lift (PP.text (Text.unpack x))



--------------------------------------------------------------------------------
instance PP Cry.TParam where
  pp = cryPP

instance PP Cry.Name where
  pp = cryPP

instance PP Cry.PrimIdent where
  pp (Cry.PrimIdent _m x) = pp x



--------------------------------------------------------------------------------
instance IsString Doc where
  fromString = lift . PP.text

instance Semigroup Doc where
  (<>) = (<.>)

instance Monoid Doc where
  mempty = lift mempty

-- | Join docuements with separation.
hsep :: [Doc] -> Doc
hsep = liftMany PP.hsep

-- | Join docuements with no separation.
hcat :: [Doc] -> Doc
hcat = liftMany PP.hcat

-- | Join vertically, no space
vcat :: [Doc] -> Doc
vcat = liftMany PP.vcat

-- | Join vertically, separated by an empty line
vsep :: [Doc] -> Doc
vsep = liftMany (PP.vcat . PP.punctuate " ")

infixl 5 $$
infixl 6 <+>, <.>

-- | Join docuements with separation.
(<+>) :: Doc -> Doc -> Doc
(<+>) = lift2 (PP.<+>)

-- | Join docuements with no separation.
(<.>) :: Doc -> Doc -> Doc
(<.>) = lift2 (PP.<>)

-- | Vertical composition
($$) :: Doc -> Doc -> Doc
($$) = lift2 (PP.$$)

-- | Indent by the given amount
nest :: Int -> Doc -> Doc
nest n = lift1 (PP.nest n)

-- | Wrap in parens
parens :: Doc -> Doc
parens = lift1 PP.parens

-- | Wrap in parens, but only if the precedence context is higher than the given
parensAfter :: Int -> Doc -> Doc
parensAfter n d =
  getPPCfg \cfg ->
    if ppPrec cfg > n then parens d else d

-- | Wrap in brackets
brackets :: Doc -> Doc
brackets = lift1 PP.brackets

-- | Wrap in braces
braces :: Doc -> Doc
braces = lift1 PP.braces

-- | Separate with commas
commaSep :: [Doc] -> Doc
commaSep = liftMany (PP.sep . PP.punctuate PP.comma)

showPP :: Show a => a -> Doc
showPP a = lift (PP.text (show a))
