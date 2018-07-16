module GoldbachList
  ( goldbachList
  ) where

-- (**) Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
prime :: Integral a => a -> Bool
prime x = and [rem x d /= 0 | d <- 2 : [3,5 .. (floor . sqrt . fromIntegral) x]]

goldbach :: Integral a => a -> (a, a)
goldbach x = goldbach' 2 (x - 2)
  where
    goldbach' a b
      | prime a && prime b = (a, b)
      | otherwise = goldbach' (a + 1) (b - 1)

goldbachList :: Integral a => a -> a -> [(a, a)]
goldbachList lower upper
  | odd lower = goldbach <$> [lower + 1,lower + 3 .. upper]
  | otherwise = goldbach <$> [lower,lower + 2 .. upper]

goldbachList' :: Integral a => a -> a -> a -> [(a, a)]
goldbachList' lower upper condition =
  filter (\(a, b) -> a > condition && b > condition) $ goldbachList lower upper
