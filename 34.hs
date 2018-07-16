module Totient
  ( phi
  ) where

-- Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
phi x =
  sum $
  (\n ->
     if coprime n x
       then 1
       else 0) <$>
  [1 .. x]
  where
    coprime a b = gcd a b == 1
