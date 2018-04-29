myButLast :: [a] -> a
myButLast (x:[]) = x
myButLast (x:y:[]) = x
myButLast (x:y:xs) = myButLast xs
