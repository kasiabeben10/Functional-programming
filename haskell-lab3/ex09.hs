--import Data.List

sumWith g []     = 0
sumWith g (x:xs) = g x + sumWith g xs -- (+) (g x) (sumWith g xs)

prodWith g []     = 1
prodWith g (x:xs) = g x * prodWith g xs -- (*) (g x) (prodWith g xs)


sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x + acc) g xs

prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' = go 1
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x * acc) g xs


foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

sumWith'' g  = foldr' (\x acc -> g x + acc) 0
prodWith'' g = foldr' (\x acc -> g x * acc) 1

foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f z [] = z
foldl'' f z (x:xs) = foldl'' f (f z x) xs

sumWith''' g  = foldl' (\acc x -> g x + acc) 0
prodWith''' g = foldl' (\acc x -> g x * acc) 1

f = foldr' (+) 0 [1..10^6]
g = foldr (+) 0 [1..10^6] --foldr z biblioteki standardowej

-- f1 = foldr (++) [] ["My", "name", "is", "Inigo", "Montoya"]
-- f2 = foldr (\x acc -> x ++ " " ++ acc) [] ["My", "name", "is", "Inigo", "Montoya"]
-- f3 = foldr1 (\x acc -> x ++ " " ++ acc) ["My", "name", "is", "Inigo", "Montoya"]

-- f4 = foldr (\_ acc -> 1 + acc) 0 [1..5]

-- f5 = foldr (:) [] [1..5]   --[1,2,3,4,5]
-- f6 = foldl (:) [] [1..5]   --error
-- f7 = foldl (\acc x -> x : acc) [] [1..5]    --[5,4,3,2,1]
-- f8 = foldr (\x xs -> xs ++ [x]) [] [1..5]    --[5,4,3,2,1]


-- f9 = foldr1 max [1,4,2,6,5,3]  --6
-- f10 = foldr1 min [1,4,2,6,5,3]   --1

-- f11 = foldr (||) False [True, False, True, False] --True
-- f12 = foldr (&&) True [True, False, True, False]  --False

-- f13 = foldr (+) 0 [1..5] == foldl (+) 0 [1..5] --True
-- f14 = foldr (*) 0 [1..5] == foldl (*) 0 [1..5] --True
-- f15 = foldr (-) 0 [1..5] == foldl (-) 0 [1..5] --False 3 !=-15


-- f16 = foldr (-) 0 [3,2,1]  --2
-- f17 = foldr1 (-) [3,2,1]   --2
-- f18 = foldl (-) 0 [3,2,1]  --6
-- f19 = foldl1 (-) 0 [3,2,1] --error

--foldr (\x acc -> "(" ++ x ++ " f " ++ acc ++ ")") "z" ["1","2","3"]
--"(1 f (2 f (3 f z)))"

--foldr1 (\x acc -> "(" ++ x ++ " f " ++ acc ++ ")") ["1","2","3"]
--"(1 f (2 f 3))"

--foldl (\acc x -> "(" ++ acc ++ " f " ++ x ++ ")") "z" ["1","2","3"]
--"(((z f 1) f 2) f 3)"

--foldl1 (\acc x -> "(" ++ acc ++ " f " ++ x ++ ")") ["1","2","3"]
--"((1 f 2) f 3)"