--ex7

not' :: Bool -> Bool
not' True = False
not' False = True

isItTheAnswear :: String -> Bool
isItTheAnswear "Love" = True
isItTheAnswear _ = False

or' :: (Bool, Bool) -> Bool
or' (x, y) = x==True || y==True

and' :: (Bool, Bool) -> Bool
and' (x,y) = x==True && y==True

nand' :: (Bool, Bool) -> Bool
nand' (x,y) = x==False || y==False

xor' :: (Bool, Bool) -> Bool
xor' (False, False) = True
xor' _ = False