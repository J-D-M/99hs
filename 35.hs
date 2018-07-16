module Primes
  ( primeFact
  ) where

-- Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
primeFact :: Integral a => a -> [a]
primeFact = primeFact' 2
  where
    primeFact' d num
      | num == 1       = []
      | rem num d == 0 = d : primeFact' d (num `div` d)
      | otherwise      = primeFact' (d + 1) num
