-- ghci> type t1 = Int  --error
-- ghci> type T1 = Int  --ok
-- ghci> type T2 = (T1,Double) --ok
-- ghci> type T3 = (a,a)      --error
-- ghci> type T4 a = (a,a,a)    --ok
-- ghci> type T5 a = [a]        --ok
-- ghci> type T6 a b = [([a],[b])]  --ok
-- ghci> type T7 a b = a -> b  --ok
-- ghci> type T8 a b = a -> (a -> b) -> Int --ok
-- ghci> type T9 a = (a, T9 a)  --error(cycle in type declaration)
-- ghci> type T10 = (Int, T10)  --error(cycle..)

polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r,phi) = (r * cos phi, r * sin phi)

-- let (x1,y1) = polarToCartesian (1,pi/4)
-- (x1, y1) = (0.7071067811865476,0.7071067811865475)

-- let (x2,y2) = polarToCartesian (x1,y1) -- !!!
--(x2, y2) = (0.5375741099526127,0.4593626849327842)

-- polarToCartesian . polarToCartesian $ (1,pi/4) -- !!!
-- (0.5375741099526127,0.4593626849327842)

type CartesianCoord' a = (a,a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r,phi) = (r * cos phi, r * sin phi)

--let (x1,y1) = polarToCartesian' (1,pi/4)
--(x1, y1) = (0.7071067811865476,0.7071067811865475)

--let (x2,y2) = polarToCartesian' (x1,y1) -- !!!
--(x2, y2) = (0.5375741099526127,0.4593626849327842)

--polarToCartesian' . polarToCartesian' $ (1,pi/4) -- !!
--(0.5375741099526127,0.4593626849327842)

newtype CartesianCoord'' a = MkCartesianCoord'' (a,a)
newtype PolarCoord'' a = MkPolarCoord'' (a,a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r,phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)


personInfoToString :: (String,String,String) -> String
personInfoToString (nm,snm,addr) =
 "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String
personInfoToString' :: PersonInfoToStringType'
personInfoToString' (nm,snm,addr) =
 "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr