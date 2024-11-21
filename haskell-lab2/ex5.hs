triples :: [(Int, Int,  Int)]
triples = [(a,b,c) | a<-[1..100], b<-[1..100], c<-[1..100], a^2 + b^2 == c^2]

how_many_triples :: Int
how_many_triples = length(triples)

--not effective way
isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []


primeNumbers :: [Int]
primeNumbers = eratoSieve [2..]
    where 
        eratoSieve :: [Int] -> [Int]
        eratoSieve (p : xs) = p : eratoSieve[x | x<-xs, x `mod` p /= 0]

isPrime2 :: Int -> Bool
isPrime2 x = x `elem` (take x primeNumbers)

howManyPrimesLowerThan10000 :: Int
howManyPrimesLowerThan10000 = length(takeWhile (<= 10000) primeNumbers)

howManyPrimesLowerThanN :: Int -> Int
howManyPrimesLowerThanN x = length(takeWhile (<= x) primeNumbers)

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (==x) xs
