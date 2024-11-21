
f1 ::  Num a => a -> a -> a
f1 = \x -> \y -> x + y

f2 ::  Num a => a -> a -> a
f2 = \x y -> x + y

f3 :: Num a => a -> a -> a
f3 x = \y -> x + y

f4 :: Num a => a -> a -> a
f4 x y = x + y

f5 :: Num a => (a, a) -> a
f5 = \(x,y) -> x + y

f6 :: Num a => (a, a) -> a
f6 (x, y) = x + y


-- f1'(x) = x-2
f1' :: Num a => a -> a
f1' = \x -> x - 2

-- f2'(x) = sqrt(x^2+y^2)
f2' :: Double -> Double -> Double
f2' = \x y -> sqrt $ x^2 + y^2

-- f3'(x) = sqrt(x^2+y^2+z^2)
f3' :: Integer -> Integer -> Integer -> Double
f3' = \x y z -> sqrt $ fromIntegral $ x^2 + y^2 + z^2

--(2*)
lamb1 :: Num a => a -> a
lamb1 = \x -> (2*) x

--(*2)
lamb2 :: Num a => a -> a
lamb2 = \x -> (*2) x

--(2^)
lamb3 :: (Integral b, Num a) => b -> a
lamb3 = \x -> (2^) x

--(^2)
lamb4 :: Num a => a -> a
lamb4 = \x -> (^2) x

--(2/)
lamb5 :: Fractional a => a -> a
lamb5 = \x -> (2/) x

--(/3)
lamb6 :: Fractional a => a -> a
lamb6 = \x -> (/3) x

--(4-)
lamb7 :: Num a => a -> a
lamb7 = \x -> (4-) x


--sqrt(x)
sqrt' :: Floating a => a -> a
sqrt' = \x -> sqrt x

--abs(x)
abs' :: Num a => a -> a
abs' = \x -> abs x

--log(x)
log' :: Floating a => a -> a
log' = \x -> log x

--id
id' :: a -> a
id' = \x -> id x

--const
const' :: a -> b -> a
const' = \x y -> x

--f7 x = if x `mod` 2 == 0 then True else False
f7 :: Integer -> Bool
f7 = \x -> x `mod` 2 == 0

--f8 x = let y = sqrt x in 2 * y^3 * (y + 1)
f8 :: Floating a => a -> a
f8 = \x -> 2 * (sqrt x)^3 * (sqrt x + 1)

--f9 1 = 3
--f9 _ = 0
f9 :: (Num a, Eq a, Num b) => a -> b
f9 = \x -> if x == 1 then 3 else 0
