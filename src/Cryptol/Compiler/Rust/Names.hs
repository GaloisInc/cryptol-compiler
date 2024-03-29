-- | Translation of names from IR to Rust.
module Cryptol.Compiler.Rust.Names
  ( RustIdent(..)
  , TraitLengthName(..)
  , SizeParamName(..)
  , rustIdentAvoiding
  , changeIdent
  , snakeCase
  , screamingSnakeCase
  , upperCamelCase
  , rustModNameChunks

    -- * Commonly used names
  , cryptolCrateString
  , cryptolCrate
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Char (isAlphaNum,isUpper,toLower,toUpper)
import Data.Maybe (mapMaybe)
import Data.List(groupBy)

import Language.Rust.Data.Ident qualified as Rust

import Cryptol.Utils.Ident qualified as Cry
import Cryptol.TypeCheck.Type qualified as Cry
import Cryptol.ModuleSystem.Name qualified as Cry
import Cryptol.Compiler.Error(panic)
import Cryptol.Compiler.IR.Cryptol

-- | Qualifier to use when accessing RTS names
cryptolCrateString :: String
cryptolCrateString = "cry_rts"

-- | Qualifier to use when accessing RTS names
cryptolCrate :: Rust.Ident
cryptolCrate = Rust.mkIdent cryptolCrateString


-- | Pick a name for something, so that it does not clash with the given names.
rustIdentAvoiding ::
  (String -> String) ->
  Set Rust.Ident {- Avoid these -} ->
  [Rust.Ident] {- Variations on the name, pick first that doesn't clash -} ->
  Rust.Ident
rustIdentAvoiding norm avoid names =
    head [ x | x <- variants, not (x `Set.member` avoid) ]
    where
    variants  = names ++ concatMap variant [ 1 .. ]
    variant i = [ Rust.mkIdent (norm (Rust.name name ++ "_" ++ show (i :: Int)))
                | name <- names
                ]

rustModNameChunks :: Cry.ModName -> [Rust.Ident]
rustModNameChunks = map cvt . Cry.modNameChunks
  where
  cvt = Rust.mkIdent . check . snakeCase . escString
  check y = if y == "lib" then "cry_lib" else y



--------------------------------------------------------------------------------
-- Cases

-- | Change a rust identifier.  Preserves the rawness.
changeIdent :: (String -> String) -> Rust.Ident -> Rust.Ident
changeIdent f n = (Rust.mkIdent (f (Rust.name n))) { Rust.raw = Rust.raw n }
-- We preserve the rawness just in case

-- | Use snake_case
snakeCase :: String -> String
snakeCase = dropWhile (== '_') . lower
  where
  lower xs =
    case xs of
      x : more
        | isUpper x -> upper (toLower x) [] more
        | otherwise -> x : lower more
      [] -> []

  upper b buf xs =
    case xs of
      x : more
        | isUpper x -> upper (toLower x) (b : buf) more
      [] -> "_" ++ reverse (b:buf)
      _ | null buf -> "_" ++ [b] ++ lower xs
        | otherwise -> "_" ++ reverse buf ++ "_" ++ [b] ++ lower xs


-- | Use SCREAMING_SNAKE_CASE
screamingSnakeCase :: String -> String
screamingSnakeCase = map toUpper . snakeCase

-- | Use UpperCamelCase
upperCamelCase :: String -> String
upperCamelCase = concat . mapMaybe check . groupBy isUnder2
  where
  isUnder x = x == '_'
  isUnder2 x y = isUnder x == isUnder y

  check g =
    case g of
      '_' : _ -> Nothing
      cs      -> Just (toUpper (head cs) : map toLower (tail cs))


--------------------------------------------------------------------------------

-- | Things that provide names.
class RustIdent a where

  -- | Pick a Rust identifier for the given thing string.
  -- We compute a non-empty list of options, where the
  -- identifiers earlier on are to be preferred.
  rustIdent :: a -> [Rust.Ident]


-- | Name of the length parameter for the methods that require a dynamic length.
newtype TraitLengthName = TraitLengthName Cry.TParam

instance RustIdent TraitLengthName where
  rustIdent (TraitLengthName tp) =
    map (changeIdent ((++ "_len") . snakeCase)) (rustIdent tp)


-- | A name of a size parameter, don't use camel case.
newtype SizeParamName = SizeParamName Cry.TParam

instance RustIdent SizeParamName where
  rustIdent (SizeParamName tp) =
      map (changeIdent snakeCase)
    $ maybe [ Rust.mkIdent "sz" ] rustIdent
    $ Cry.tpName tp

instance RustIdent Cry.TParam where
  rustIdent tp =
      map (changeIdent upperCamelCase)
    $ maybe [ Rust.mkIdent "T" ] rustIdent
    $ Cry.tpName tp


instance RustIdent a => RustIdent (IRFunName a) where
  rustIdent = rustIdent . irfnName
  -- XXX: Use instance to pick a more readable name
  -- Currently we use the same name for all instances, relying
  -- on disambiguation to pick different names

instance RustIdent a => RustIdent (IRFunNameFlavor a) where
  rustIdent i =
    map (changeIdent snakeCase)
    case i of
      IRPrimName p -> rustIdent p
      IRDeclaredFunName x -> rustIdent x

instance RustIdent IRPrim where
  rustIdent i =
    case i of
      CryPrim p -> rustIdent p
      _         -> []   -- These are not declared by declarations

instance RustIdent NameId where
  rustIdent nid =
    case nid of
      NameId x -> rustIdent x
      AnonId _ -> rustIdent ("anon" :: Text)

instance RustIdent Cry.Name where
  rustIdent nm =
    map (changeIdent snakeCase)
    case Cry.nameInfo nm of
      Cry.LocalName _ i -> rustIdent i
      Cry.GlobalName _ og -> rustIdent og

instance RustIdent Cry.OrigName where
  rustIdent og =
    let i = Cry.ogName og
        (_top,nested) = Cry.modPathSplit (Cry.ogModule og)
        qualName = Rust.name . head . rustIdent
        base = case rustIdent i of
                 [b] -> b
                 _   -> panic "rustIdent@OrigName" ["Expecetd 1"]
        qual prev q = Rust.mkIdent (qualName q ++ "_" ++ Rust.name prev)
    in scanl qual base nested

instance RustIdent Cry.PrimIdent where
  rustIdent (Cry.PrimIdent _m txt) = rustIdent txt

instance RustIdent Cry.Ident where
  rustIdent = rustIdent . Cry.identText

instance RustIdent Text where
  rustIdent name
    | name `Set.member` rustKeywords = [(Rust.mkIdent str) { Rust.raw = True }]
    | Just i <- Map.lookup name knownOperators = [Rust.mkIdent i]
    | otherwise = [Rust.mkIdent (dflt (escString str))]
    where
    str = Text.unpack name
    dflt x = if null x then "x" else x


--------------------------------------------------------------------------------

escString :: String -> String
escString = concat . zipWith escChar (True : repeat False)

escChar :: Bool -> Char -> String
escChar isFirst c
  | isAlphaNum c || c == '_'            = [c]
  | Just i <- Map.lookup c symbolNames  = sep i
  | otherwise                           = []
  where
  sep str = if isFirst then str else "_" ++ str

knownOperators :: Map Text String
knownOperators = Map.fromList
  [ "==>"   ~> "cry_implies"
  , "\\/"   ~> "cry_logic_or"
  , "/\\"   ~> "cry_logic_and"
  , "=="    ~> "cry_equal"
  , "==="   ~> "cry_equal_fun"
  , "!="    ~> "cry_not_equal"
  , "!=="   ~> "cry_not_equal_fun"

  , ">"     ~> "cry_gt"
  , ">="    ~> "cry_geq"
  , "<"     ~> "cry_lt"
  , "<="    ~> "cry_leq"
  , "<$"    ~> "cry_signed_lt"
  , ">$"    ~> "cry_signed_gt"
  , "<=$"   ~> "cry_signed_leq"
  , ">=$"   ~> "cry_signed_geq"

  , "||"    ~> "cry_bit_or"
  , "^"     ~> "cry_bit_xor"
  , "^^"    ~> "cry_bit_and"

  , "#"     ~> "cry_append"

  , "<<"    ~> "cry_shift_left"
  , "<<<"   ~> "cry_rotate_left"
  , ">>"    ~> "cry_shift_right"
  , ">>>"   ~> "cry_rotate_right"

  , "+"     ~> "cry_add"
  , "-"     ~> "cry_sub"
  , "*"     ~> "cry_mul"
  , "/"     ~> "cry_div"
  , "/$"    ~> "cry_signed_div"
  , "/^"    ~> "cry_div_up"
  , "%"     ~> "cry_mod"
  , "%$"    ~> "cry_signed_mod"
  , "%^"    ~> "cry_mod_up"

  , "^^"    ~> "cry_exp"

  , "@"     ~> "cry_at"
  , "@@"    ~> "cry_at_seq"
  , "!"     ~> "cry_back_at"
  , "!!"    ~> "cry_back_at_seq"
  ]
  where (~>) = (,)

symbolNames :: Map Char String
symbolNames = Map.fromList
  [ '!'   ~> "bang"
  , '#'   ~> "hash"
  , '$'   ~> "dollar"
  , '%'   ~> "percent"
  , '&'   ~> "amp"
  , '*'   ~> "star"
  , '+'   ~> "plus"
  , '-'   ~> "dash"
  , '.'   ~> "dot"
  , '/'   ~> "fslash"
  , ':'   ~> "colon"
  , '<'   ~> "lt"
  , '='   ~> "eq"
  , '>'   ~> "gt"
  , '?'   ~> "question"
  , '@'   ~> "at"
  , '\\'  ~> "bslash"
  , '^'   ~> "hat"
  , '|'   ~> "bar"
  , '~'   ~> "tilde"
  ]
  where (~>) = (,)

rustKeywords :: Set Text
rustKeywords = Set.fromList
  [ "as"
  , "abstract"
  , "async"
  , "await"
  , "become"
  , "box"
  , "break"
  , "const"
  , "continue"
  , "crate"
  , "do"
  , "dyn"
  , "else"
  , "enum"
  , "extern"
  , "false"
  , "final"
  , "fn"
  , "for"
  , "if"
  , "impl"
  , "in"
  , "let"
  , "loop"
  , "macro"
  , "match"
  , "mod"
  , "move"
  , "mut"
  , "override"
  , "priv"
  , "pub"
  , "ref"
  , "return"
  , "self"
  , "Self"
  , "static"
  , "struct"
  , "super"
  , "trait"
  , "true"
  , "try"
  , "type"
  , "typeof"
  , "unsafe"
  , "unsized"
  , "union"
  , "use"
  , "virtual"
  , "where"
  , "while"
  , "yield"
  ]
