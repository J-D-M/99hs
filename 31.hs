module IsPrime
  ( isPrime
  ) where

-- Determine whether a given integer number is prime

isPrime :: (Integral a) => a -> Bool
isPrime x =
  (x > 1) && and [rem x y /= 0 | y <- [2 .. (floor . sqrt . fromIntegral) x]]
