module Combinations (comb) where

comb :: Int -> [a] -> [[a]]
comb 0 _        = [[]]
comb _ []       = []
comb len (x:xs) = (map (x:) (comb (len - 1) xs)) ++ comb len xs
