fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False


fst2Div :: (Eq a, Integral a) => [a] -> Bool
fst2Div (x : y : _) | y `mod` x == 0 = True
fst2Div _                           = False


fst3Div :: (Eq a, Integral a) => [a] -> Bool
fst3Div (x : _ : y: _) | y `mod` x == 0 = True
fst3Div _                           = False
