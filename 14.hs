dupe :: [a] -> [a]
dupe []     = []
dupe (x:xs) = replicate 2 x ++ dupe xs
