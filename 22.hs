module Range where

range :: Int -> Int -> [Int]
range a b
  | a > b = []
  | a < b = a : range (a + 1) b
  | otherwise = [b]
{-
  range a b = [a..b]

  range = enumFromTo
-}
