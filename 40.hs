module Goldbach
  ( goldbach
  ) where

--  Write a predicate to find the two prime numbers that sum up to a given even integer.
prime :: Integral a => a -> Bool
prime x = and [rem x d /= 0 | d <- 2 : [3,5 .. (floor . sqrt . fromIntegral) x]]

goldbach :: Integral a => a -> Maybe (a, a)
goldbach x =
  if odd x || x < 2
    then Nothing
    else Just $goldbach' 2 (x - 2)
  where
    goldbach' a b
      | prime a && prime b = (a, b)
      | otherwise = goldbach' (a + 1) (b - 1)
