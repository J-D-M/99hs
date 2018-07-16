module PrimesCount
  ( primeC
  ) where

-- Construct a list containing the prime factors and their multiplicity.
primeFact :: Integral a => a -> [a]
primeFact = primeFact' 2
  where
    primeFact' d num
      | num == 1 = []
      | rem num d == 0 = d : primeFact' d (num `div` d)
      | otherwise = primeFact' (d + 1) num

primeC :: Integral a => a -> [(a, Int)]
primeC = primeC' . primeFact
  where
    primeC' [] = []
    primeC' ls@(x:xs) = (x, length lsHead) : primeC' lsTail
      where
        lsHead = takeWhile (== x) ls
        lsTail = dropWhile (== x) ls
