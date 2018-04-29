myLen :: [a] -> Int
myLen = go 0
  where
    go count [] = count
    go count (x:xs) = go (count + 1) xs
