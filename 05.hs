myRev :: [a] -> [a]
myRev = go []
  where
    go acc [] = acc
    go acc (x:xs) = go (x:acc) xs
