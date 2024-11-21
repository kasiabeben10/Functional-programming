sqr x = x^2

funcFactory n = case n of
    1 -> id
    2 -> sqr
    3 -> (^3)
    4 -> \x -> x^4
    5 -> intFunc
    _ -> const n
    where 
        intFunc x = x^5

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n = case n of
    0 -> \x -> 1
    1 -> \x -> 1 + x
    2 -> \x -> 1 + x + x^2/2
    3 -> \x -> 1 + x + x^2/2 + x^3/6
    4 -> \x -> 1 + x + x^2/2 + x^3/6 + x^4/24
    5 -> \x -> 1 + x + x^2/2 + x^3/6 + x^4/24 + x^5/120
    _ -> \x -> 1 + x + x^2/2 + x^3/6 + x^4/24 + x^5/120 + x^6/720

expApproxUpToPro :: Int -> Double -> Double
expApproxUpToPro n = \x -> sum [x^k / fromIntegral (factorial k) | k <- [0..n]]
  where
    factorial 0 = 1
    factorial m = m * factorial (m - 1)


dfr :: (Double -> Double) -> Double -> (Double -> Double)
dfr f h = \x -> (f (x + h) - f x) / h

dfc :: (Double -> Double) -> Double -> (Double -> Double)
dfc f h = \x -> (f (x + h) - f (x-h)) / (2*h)

d2f :: (Double -> Double) -> Double -> (Double -> Double)
d2f f h = \x -> ((dfr f h (x+h)) - (dfr f h x)) / h
    where dfr f h = \y -> (f (y + h) - f y) / h

d2f' :: (Double -> Double) -> Double -> (Double -> Double)
d2f' f h = \x -> (f (x + h) - 2 * f x + f (x - h)) / (h^2)