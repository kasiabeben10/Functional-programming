import Data.List

isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = foldr (&&) True (zipWith (==) (sort xs) xs)  
-- isSortedAsc [1,2,2,3] -> True, isSortedAsc [1,2,1] -> False

everySecond :: [t] -> [t]
everySecond xs = map (\(x,y) -> x) (filter(\(x,y)-> odd y) (zip xs [1..])) 
-- everySecond [1..8] -> [1,3,5,7]

