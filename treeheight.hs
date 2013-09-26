data Tree a = Node a (Tree a) (Tree a)
    | Empty
      deriving (Show)

height Empty = 0
height (Node a l r)
    | height l < height r = 1 + (height r)
    | otherwise = 1 + (height l)

main = do
    print (height (Node "a" (Node "b" Empty (Node "c" Empty Empty)) (Node "d" (Node "e" (Node "f" Empty Empty) Empty) Empty)))


