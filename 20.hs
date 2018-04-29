remove_at :: Eq a => [a] -> Int -> [a]
remove_at ls@(x:xs) n
  | ls == []   = []
  | n > 1      = x:remove_at xs (n - 1)
  | otherwise  = xs
