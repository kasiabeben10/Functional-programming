--ex8

--not' :: Bool -> Bool
--not' b = case b of
--	True -> False
--	False -> True

absInt n = case (n >= 0) of
	True -> n
	_    -> -n

isItTheAnswear :: String -> Bool
isItTheAnswear ans = case ans of
		"Love" -> True
		_ -> False

not' :: Bool -> Bool
not' n = case n of
	True -> False
	_ -> True

or' :: (Bool, Bool) -> Bool
or' (x,y) = case (x||y) of
	False -> False
	_ -> True

and' :: (Bool, Bool) -> Bool
and' (x,y) = case (x && y) of
	True -> True
	_ -> False

nand' :: (Bool, Bool) -> Bool
nand' (x,y) = case (x && y) of
	True -> False
	_ -> True

xor' :: (Bool, Bool) -> Bool
xor' (x,y) = case (x || y) of
	False -> True
	_ -> False