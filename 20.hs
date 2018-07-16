module RemoveAt
  ( removeAt
  ) where

removeAt :: [a] -> Int -> [a]
removeAt ls@(x:xs) n
  | null ls = []
  | n > 1 = x : removeAt xs (n - 1)
  | otherwise = xs
