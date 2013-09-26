intersperse' sep [] = []
intersperse' sep [x] = x
intersperse' sep (x:xs) = x ++ [sep] ++ intersperse' sep xs

main = do
    print (intersperse' ',' ["foo","bar"])
    print (intersperse' ',' [])
    print (intersperse' ',' ["foo"])
