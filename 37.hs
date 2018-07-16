module TotientImproved
  ( phiImproved
  ) where

-- Calculate Euler's totient function phi(m) (improved)
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

phiImproved :: Integral a => a -> a
phiImproved x = product [(p - 1) * p ^ (c - 1) | (p, c) <- primeC x]
