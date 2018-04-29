index :: [a] -> Int -> a
index (l:_) 1 = l
index (x:xs) a = index xs (a - 1)
