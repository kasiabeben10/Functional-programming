import Data.Char

doubleElems []     = []
doubleElems (x:xs) = 2 * x : doubleElems xs


sqrElems [] = []
sqrElems (x:xs) = x^2 : sqrElems xs

lowerCase []  = []
lowerCase (x:xs) = toLower x : lowerCase xs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x) : map' f xs

doubleElems' = map' (\x -> 2*x)
sqrElems' = map' (\x -> x^2)
lowerCase' = map' (\x -> toLower x)

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = [f x | x <- xs]

doubleElems'' = map'' (\x -> 2*x)

sqrElems''   = map'' (\x -> x^2)

lowerCase''  = map'' (\x -> toLower x)

f = length . filter even $ doubleElems [1..10^7]
g = length . filter even . map (*2) $ [1..10^7] -- map z biblioteki standardowej

