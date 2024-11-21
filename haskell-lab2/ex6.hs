fib :: (Num a, Eq a) => a -> a
fib n =
    if n == 0 || n == 1 then n
    else fib (n - 2) + fib (n - 1)

fib2 :: Num a => Int -> a
fib2 n = 
    if n == 0 || n == 1 then 0
    else last(take(n+1) fibs)
        where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a
prod' [x] = x
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool
and' [x] = x
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs) = n==x || elem' n xs

doubleAll :: Num t => [t] -> [t]
doubleAll [] = []
doubleAll (x:xs) = [2*x] ++ doubleAll xs

squareAll :: Num t => [t] -> [t]
squareAll [] = []
squareAll (x:xs) = [x^2] ++ squareAll xs

selectEven :: Integral t => [t] -> [t]
selectEven [] = []
selectEven (x:xs) = if x `mod` 2 == 0 then [x] ++ selectEven xs
                    else selectEven xs