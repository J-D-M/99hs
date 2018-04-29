data Nested a = Elem a | List [Nested a]

flatten :: Nested a -> [a]
flatten (List [])     = []
flatten (Elem x)      = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
