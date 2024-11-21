isPanlindrome :: [Char] -> Bool
isPanlindrome s = s == reverse s

getElemAtIdx :: Int -> [a] -> a
getElemAtIdx x xs = head (drop x xs)

capitalize :: [Char] -> [Char]
capitalize w = (toEnum(fromEnum(head w) - 32)) : (drop 1 w)