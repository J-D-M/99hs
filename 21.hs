module InsertAt where

insertAt :: Eq a => a -> [a] -> Int -> [a]
insertAt e ls@(x:xs) i
  | null ls   = [e]
  | i > 1     = x : insertAt e xs (i - 1)
  | otherwise = e : ls
