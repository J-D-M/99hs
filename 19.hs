rotate :: Eq a => [a] -> Int -> [a]
rotate ls n = take (length ls) . drop (mod n (length ls)) $ cycle ls
