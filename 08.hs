compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs)
  | xs == []      =  x : []
  | x == head xs  =  compress xs
  | otherwise     =  x : compress xs
