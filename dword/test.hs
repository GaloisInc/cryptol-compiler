import Data.Bits
import Debug.Trace

example = pmod 32 12345678 123

pmod :: Int -> Integer -> Integer -> Integer
pmod w x m = trace msg go degree (x .&. mask) (clearBit m degree)
    where
      msg = unlines
              [ "degree  = " ++ show degree
              ]
      degree :: Int
      degree = fromInteger (widthInteger m - 1)

      reduce :: Integer -> Integer
      reduce u = if testBit u degree then trace ("reducing ++ " ++ show u ++ " ~> " ++ show (u `xor` m)) (u `xor` m) else u
      {-# INLINE reduce #-}

      mask = bit degree - 1

      -- invariant: z and p are in the range [0..mask]
      go !i !z !p
        | trace ("i = " ++ show i ++ ", result = " ++ show z ++ ", p = " ++ show p) False = undefined
        | i < w     = go (i+1) (if testBit x i then z `xor` p else z) (reduce (p `shiftL` 1))
        | otherwise = z


-- | Compute the number of bits required to represent the given integer.
widthInteger :: Integer -> Integer
widthInteger x = go' 0 (if x < 0 then complement x else x)
  where
    go s 0 = s
    go s n = let s' = s + 1 in s' `seq` go s' (n `shiftR` 1)

    go' s n
      | n < bit 32 = go s n
      | otherwise  = let s' = s + 32 in s' `seq` go' s' (n `shiftR` 32)



