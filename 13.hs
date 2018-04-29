data LS a = Single a | Multiple Int a
  deriving Show

directEnc :: Eq a => [a] -> [LS a]
directEnc [] = []
directEnc ls = go first : directEnc second
  where
    (first, second) = span (== head ls) ls
    go xs           =
      if length xs > 1
         then Multiple (length xs) (head xs)
      else Single (head xs)
