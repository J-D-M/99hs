insert_at :: Eq a => a -> [a] -> Int -> [a]
insert_at e ls@(x:xs) i
  | ls == []   =  [e]
  | i > 1      =  x : insert_at e xs (i - 1)
  | otherwise  =  e:ls
