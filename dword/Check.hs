{-# Language ScopedTypeVariables #-}
import Data.Foldable
import Control.Monad

main =
  do xs <- map read . lines <$> getContents
     forM_ xs $ \case_@(sz :: Integer, lhs :: Integer, rhs :: Integer, ans :: Integer) ->
       unless ( (lhs*rhs) `mod` (2^sz) == ans ) $
         print case_
