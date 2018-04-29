split :: Eq a => [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split ls@(x:xs) n
  | n > 0         = (x : first, second)
  | otherwise     = ([], ls)
  where
    (first, second) = split xs (n - 1)
