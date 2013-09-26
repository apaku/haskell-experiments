length' []   = 0
length' (x:xs) = 1 + (length' xs)

main = do
    print (length' [1,2,3,4,5])
    print (length' [])
    print (length' [1])
    print (length' "abc")
