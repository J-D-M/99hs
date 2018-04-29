pack [] = []
pack ls = front : pack back
  where
    (front, back) = span (== (head ls)) ls
