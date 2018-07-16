module PrimeList
  ( pList
  ) where

-- Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
sieve :: Integral a => [a]
sieve = sieve' (2:[3, 5 ..])
  where
    sieve' (p:ps) = p : sieve' [x | x <- ps, rem x p > 0]

pList :: Integral a => a -> a -> [a]
pList lower upper = (takeWhile (< upper) . dropWhile (< lower)) sieve
