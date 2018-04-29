-- problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack ls = front : pack back
  where
    (front, back) = span (== (head ls)) ls
--

rlEncode' :: [a] -> (Int, a)
rlEncode' x = (length x, head x)

rlEncode :: Eq a => [a] -> [(Int, a)]
rlEncode = (map rlEncode') . pack
