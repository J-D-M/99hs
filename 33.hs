module Coprime
  ( coprime
  ) where

-- Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
coprime :: Integral a => a -> a -> Bool
coprime a b = gcd a b == 1
