mean' [] = 0
mean' (x:xs) = (fromIntegral (x + sum xs)) / (fromIntegral (1 + length xs))

main = do
    print (mean' [1,2,4])
    print (mean' [1,4])
    print (mean' [14,11,23])
    print (mean' [2])
    print (mean' [])
    print (mean' [1,2,3,4,4,6,6,2])

