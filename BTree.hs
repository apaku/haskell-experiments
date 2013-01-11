import Data.List

data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Read, Eq, Show)

instance Functor BTree where
    fmap f Empty = Empty
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)


folderlike :: (Show a) => BTree a -> String
folderlike Empty = ""
folderlike (Node x left right) = unlines ( ( (map (g) (lines (folderlike left))) ++ [show x] ++ (map (g) (lines (folderlike right))) ) )
    where g a = "  " ++ a

pretty :: (Show a) => BTree a -> String
pretty = unlines . transpose . lines . folderlike

treeInsert :: (Ord a) => a -> BTree a -> BTree a
treeInsert val Empty = Node val Empty Empty
treeInsert val (Node elem left right) 
    | val < elem = Node elem ( treeInsert val left ) right
    | val == elem = Node elem left right
    | val > elem = Node elem left ( treeInsert val right )

contains :: (Ord a) => a -> BTree a -> Bool
contains val Empty = False
contains val (Node elem left right) 
    | val == elem = True
    | val < elem = contains val left
    | val > elem = contains val right

parent :: (Eq a, Ord a) => a -> BTree a -> Maybe a
parent val Empty = Nothing
parent val (Node x Empty Empty) = Nothing
parent val (Node x Empty (Node z zl zr))
    | val == z = Just x
    | otherwise = parent val (Node z zl zr)

parent val (Node x (Node z zl zr) Empty)
    | val == z = Just x
    | otherwise = parent val (Node z zl zr)
    
parent val (Node x (Node y yl yr) (Node z zl zr))
    | val == z = Just x
    | val == y = Just x
    | val < x = parent val (Node y yl yr)
    | val > x = parent val (Node z zl zr)
    | otherwise = Nothing

height :: BTree a -> Int
height Empty = 0
height (Node x left right) = (max (height left) (height right) ) + 1

linearize :: BTree a -> [(a,Int)]
linearize Empty = []
linearize (Node x left right) = (x, height (Node x left right)):((linearize left) ++ (linearize right))

maxwidth :: BTree a -> Int
maxwidth x = 2 ^ (height x) - 1

width a b = (+) (abs ((-) a b )) 1
halfwidth a b = div (width a b) 2
leftstart a b = a
leftend a b = (-) ((+) a (halfwidth a b)) 1
rightstart a b = (+) ((-) b (halfwidth a b)) 1
rightend a b = b

foo :: Int -> Int -> BTree a -> [(a,Int,Int)]
foo s e Empty = []
foo s e (Node x left right) = (x,s,e):(foo (leftstart s e) (leftend s e) left) ++ (foo (rightstart s e) (rightend s e) right)

main = do
        putStrLn (show k)
        putStrLn (show ((read (show k)) == k))
        putStrLn "==============================================="
        putStr (folderlike k)
        putStrLn "\n-------------------------------------------"
        putStr (pretty k)
        putStrLn "==============================================="
        putStr (folderlike z)
        putStrLn "\n-------------------------------------------"
        putStr (pretty z)
            where k = foldr (treeInsert) Empty [5,1,6,4,10,11,12,2,7,3]
                  z = foldr (treeInsert) Empty [8,6,4,1,7,3,5]

