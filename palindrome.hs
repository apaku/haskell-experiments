palindrome [x] = [x,x]
palindrome (x:xs) = [x] ++ (palindrome xs) ++ [x]

-- isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = False
isPalindrome (x:xs)
     | x == last xs = isPalindrome (take ((length xs) - 1) xs)
     | otherwise = False

main = do
    print (palindrome [1,2,3])
    print (palindrome "an")
    print (palindrome [1])
    print (palindrome [4,6,1,7])
    print (isPalindrome [1,2,3,3,2,1])
    print (isPalindrome "anna")
    print (isPalindrome [1])
    print (isPalindrome [1,2,3])
