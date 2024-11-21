fiveToPower_ :: Integer -> Integer
fiveToPower_ = (5 ^)

_toPower5 :: Num a => a->a
_toPower5 = (^ 5)

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 = (5 -)

subtr5From_ :: Num a => a -> a
subtr5From_ = flip (-) 5

-- to samo co flip
flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f x y = f y x

--odpowiednik flip2 dla funkcji trójparametrowej
flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 :: f x y z = f z y x

