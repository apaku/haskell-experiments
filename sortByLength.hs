import Data.List (sortBy)
compareByLength l1 l2
    | length l1 < length l2 = LT
    | length l1 > length l2 = GT
    | otherwise = EQ
sortByLength x = sortBy compareByLength x

main = do
    print (sortByLength [[1,2,3],[1,2],[2],[],[2,4,5,6,2,1],[]])
    print (sortByLength [[2],[1]])
