neverEndingStory :: (Integral a, Num t) => t -> a
neverEndingStory x = neverEndingStory (x + 1) `mod` 100

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)