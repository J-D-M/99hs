slice :: Eq a => [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) a b
  | a > 1     = slice xs (a - 1) (b - 1)
  | b > 0     = x : slice xs a (b - 1)
  | otherwise = []
