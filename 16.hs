dropEvery :: Eq a => [a] -> Int -> [a]
dropEvery ls n = dropEvery' ls n 1
  where
    dropEvery' :: Eq a => [a] -> Int -> Int -> [a]
    dropEvery' [] _ _ = []
    dropEvery' (x:xs) n c
      | rem c n == 0    = dropEvery' xs n (c + 1)
      | otherwise       = x : dropEvery' xs n (c + 1)
