module GCD
  ( myGcd
  ) where

--  Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
myGcd :: Integral a => a -> a -> a
myGcd x y =
  if y == 0
    then abs x
    else myGcd y (rem x y)
