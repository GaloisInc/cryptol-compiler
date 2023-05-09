-- | Translation of names from IR to Rust.
module Cryptol.Compiler.Rust.Names (rustIdent, rustUniqueIdent) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Char (isAlphaNum)

import Language.Rust.Data.Ident qualified as Rust

-- | Pick a Rust identifier for the given textual string.
rustIdent :: Text -> Rust.Ident
rustIdent name
  | name `Set.member` rustKeywords = (Rust.mkIdent str) { Rust.raw = True }
  | Just i <- Map.lookup name knownOperators = Rust.mkIdent i
  | otherwise =
    Rust.mkIdent (dflt (concat (zipWith escChar (True : repeat False) str)))
  where
  str = Text.unpack name
  dflt x = if null x then "x" else x

-- | Generate a name variant that does not clash with the other names.
rustUniqueIdent ::
  Set Rust.Ident {- ^ Avoid these names -} ->
  Rust.Ident     {- ^ Name that should not clash -} ->
  Rust.Ident     {- ^ New name -}
rustUniqueIdent avoid name =
  head [ x | x <- variants, not (x `Set.member` avoid) ]
  where
  variants  = name : map variant [ 1 .. ]
  variant i = Rust.mkIdent (Rust.name name ++ "_" ++ show (i :: Int))


escChar :: Bool -> Char -> String
escChar isFirst c
  | isAlphaNum c                        = [c]
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
