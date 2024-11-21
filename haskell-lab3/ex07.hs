onlyEven [] = []
onlyEven (x:xs)
 | x `mod` 2 == 0 = x : onlyEven xs
 | otherwise      = onlyEven xs

--onlyOdd [1..10] -- [1,3,5,7,9]
--onlyUpper "My name is Inigo Montoya. You killed my father. Prepare to die." -- "MIMYP"

onlyOdd [] = []
onlyOdd (x:xs)
    | x `mod` 2 == 1 = x : onlyOdd xs
    | otherwise      = onlyOdd xs


onlyUpper [] = []
onlyUpper (x:xs)
    | x `elem` ['A'..'Z'] = x : onlyUpper xs
    | otherwise           = onlyUpper xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

onlyEven' = filter' (\x -> x `mod` 2 == 0)

onlyOdd' = filter' (\x -> x `mod` 2 == 1)

onlyUpper' = filter' (\x -> x `elem` ['A'..'Z'])


--length (onlyEven [1..10^6]) 1.14 s
--length (filter even [1..10^6]) 0.05s

--length (onlyEven [1..10^6]) == length $ onlyEven [1..10^6]     
--length (filter even [1..10^6]) == length $ filter even [1..10^6]

--length (filter even [1..10^6]) == foldr (+) 0 [1 | x <- [1..10^6], even x]
--foldr (+) 0 [1 | x <- [1..10^6], even x] 0.99s


--filter (\s -> length s == 2) ["a", "aa", "aaa", "b", "bb"]
--["aa","bb"] 0.00secs

--filter (\(x,y) -> x > y) [(1,2), (2,2), (2,1), (2,2), (3,2)]
--[(2,1),(3,2)] 0.01secs

--filter (\xs -> sum xs > 300) [[1..5], [56..60], [101..105]]
--[[101,102,103,104,105]] 0.01secs

--length . filter (\f -> f 2 > 10) $ [(+5), (*5), (^5), \x -> 3 * x + 7]
--2   0.01sec