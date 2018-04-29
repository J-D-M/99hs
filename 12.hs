-- problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack ls = front : pack back
  where
    (front, back) = span (== (head ls)) ls

-- problem 10

rlEncode' :: [a] -> (Int, a)
rlEncode' x = (length x, head x)

rlEncode :: Eq a => [a] -> [(Int, a)]
rlEncode = (map rlEncode') . pack

-- problem 11

data Encoded a = Single a | Multiple Int a deriving Show

modRLE' :: (Int, a) -> Encoded a
modRLE' (n, x) =
  if n > 1
     then Multiple n x
  else Single x

modRLE :: Eq a => [a] -> [Encoded a]
modRLE = (map (modRLE' . rlEncode')) . pack

--

decode :: [Encoded a] -> [a]
decode []     = []
decode (x:xs) = decode' x ++ decode xs
  where
    decode' (Single x) = [x]
    decode' (Multiple n x) = replicate n x
