qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = [ y | y <- xs, y <= x ]
   rightPart xs = [ y | y <- xs, y > x  ]


qSort' :: Ord a => [a] -> [a]
qSort' [] = []
qSort' (x:xs) = qSort' (leftPart xs) ++ [x] ++ qSort' (rightPart xs)
 where
   leftPart  xs = filter (<=x) xs
   rightPart xs = filter (>x) xs


concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (x:xs) = foldr (++) x xs

mSort :: Ord a => [a] -> [a]
mSort [] = []
mSort [x] = [x]
mSort xs = merge (mSort left) (mSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs
    merge [] xs = xs
    merge xs [] = xs
    merge (x:xs) (y:ys) = 
                        if x <= y    then x : merge xs (y:ys)
                        else y : merge (x:xs) ys