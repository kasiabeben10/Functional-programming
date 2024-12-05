-- product type example (one constructor)
data CartInt2DVec = MkCartInt2DVec Int Int -- konwencja: prefix 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

-- :i CartInt2DVec
-- type CartInt2DVec :: *
-- data CartInt2DVec = MkCartInt2DVec Int Int

-- :t CartInt2DVec  
--error:
   -- * Illegal term-level use of the type constructor or class `CartInt2DVec' 

-- :t MkCartInt2DVec
-- MkCartInt2DVec :: Int -> Int -> CartInt2DVec

-- let p12 = CartInt2DVec 1 2 --error
-- let p12 = MkCartInt2DVec 1 2  --ok

-- xCoord p12 = 1
-- yCoord p12 = 2

--xCoord $ MkCartInt2DVec 5 10
--5

-----------------------------------------
type X = Int
type Y = Int
data CartInt2DVec_v2 = MkCartInt2DVec_v2 X Y -- konwencja: prefix 'Mk' dla konstruktora

xCoord_v2 :: CartInt2DVec_v2 -> X
xCoord_v2 (MkCartInt2DVec_v2 x _) = x

yCoord_v2 :: CartInt2DVec_v2 -> Y
yCoord_v2 (MkCartInt2DVec_v2 _ y) = y
------------------------------------------


data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

-- :i Cart2DVec'
-- type Cart2DVec' :: * -> *
-- data Cart2DVec' a = MkCart2DVec' a a

-- :t MkCart2DVec'
-- MkCart2DVec' :: a -> a -> Cart2DVec' a

-- :t MkCart2DVec' 1 2
-- MkCart2DVec' 1 2 :: Num a => Cart2DVec' a

-- :t MkCart2DVec' 1.0 2.0
-- MkCart2DVec' 1.0 2.0 :: Fractional a => Cart2DVec' a 

-- :t MkCart2DVec' 1.0 2
-- MkCart2DVec' 1.0 2 :: Fractional a => Cart2DVec' a

-- :t xCoord' $ MkCart2DVec' 5 10
-- xCoord' $ MkCart2DVec' 5 10 :: Num a => a

-- yCoord' $ MkCart2DVec' 5.0 10.0
-- 10.0


data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

xCoord'' :: Cart2DVec'' a -> a
xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

yCoord'' :: Cart2DVec'' a -> a
yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal -- uwaga na kolejność x,y


-- :i Cart2DVec''
-- type Cart2DVec'' :: * -> *
-- data Cart2DVec'' a = MkCart2DVec'' {x :: a, y :: a}

-- :t MkCart2DVec''
-- MkCart2DVec'' :: a -> a -> Cart2DVec'' a

-- :t xCoord''
-- xCoord'' :: Cart2DVec'' a -> a

-- :t yCoord''
-- yCoord'' :: Cart2DVec'' a -> a

-- :t x
-- :: Cart2DVec'' a -> a
 --dlaczego ta funkcja istnieje (skoro jej nie deklarowaliśmy/definiowaliśmy)?


-- let p23 = MkCart2DVec'' {x = 2, y = 3}
--xCoord'' p23  = 2
--x p23 = 2
--yCoord'' p23 = 3
--y p23 = 3

--xCoord'' $ MkCart2DVec'' {x=1, y=2} = 1
--xCoord'' $ MkCart2DVec'' 1 2 = 1
--yCoord'' $ MkCart2DVec'' {x=1, y=2} = 2
--yCoord'' $ MkCart2DVec'' 1 2 = 2

data Cart2DVec''' a = MkCart2DVec''' {x'::a, y'::a}

-- :i Cart2DVec'''
--type Cart2DVec''' :: * -> *
--data Cart2DVec''' a = MkCart2DVec''' {x' :: a, y' :: a}

-- :t MkCart2DVec'''
--MkCart2DVec''' :: a -> a -> Cart2DVec''' a

-- :t x'
-- x' :: Cart2DVec''' a -> a
-- :t y'
-- y' :: Cart2DVec''' a -> a
-- let p23 = MkCart2DVec''' {x'=2, y'=3}
-- x' p23 = 2
-- y' p23 = 3

---------------------------------------------
-- sum type example (two constructors)
data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x


-- ghci> head' EmptyL
-- *** Exception: head': the empty list has no head!
-- ghci> head' Cons 1
--error:
    -- * Couldn't match expected type: List (t0 -> t)
      --            with actual type: a0 -> List a0 -> List a0 
-- ghci> head' (Cons 1 EmptyL)
-- 1
-- ghci> head' $ Cons 1 EmptyL
-- 1
-- ghci> Cons 1 EmptyL -- show in action :)
-- Cons 1 EmptyL
-- ghci> head' $ Cons 1 $ Cons 2 EmptyL
-- 1

--------------------------------
-- enum type example (special case of sum type)
data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"

----------------------------------
{-
uwaga: ta sama nazwa* dla:
 - konstruktora typu (po lewej)
 - konstruktora danych/wartości (po prawej)
 * druga (obok omówionej poprzednio -- z prefiksem 'Mk') powszechna konwencja w Haskellu!
-}
data Cart3DVec a = Cart3DVec a a a

xCoord3D :: Cart3DVec a -> a
xCoord3D (Cart3DVec x _ _) = x

yCoord3D :: Cart3DVec a -> a
yCoord3D (Cart3DVec _ y _) = y

zCoord3D :: Cart3DVec a -> a
zCoord3D (Cart3DVec _ _ z) = z

data Cart3DVec' a = Cart3DVec' {x'::a, y'::a, z'::a}
xCoord3D' :: Cart3DVec a -> a
xCoord3D' (Cart3DVec' x _ _) = x
yCoord3D' :: Cart3DVec a -> a
yCoord3D' (Cart3DVec' _ y _) = y
zCoord3D' :: Cart3DVec a -> a
zCoord3D' (Cart3DVec' _ _ z) = z

-------------------------------------
data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle a b) = a * b
-------------------------------------
data Tree a = EmptyT |
              Node a (Tree a) (Tree a)
              deriving Show

rootValue :: Tree a -> a
rootValue (EmptyT) = error "empty tree"
rootValue (Node x _ _) = x
-------------------------------------
data TrafficLights = Red | Yellow | Green

actionFor :: TrafficLights -> String
actionFor Red = "Stop"
actionFor Yellow = "Get ready"
actionFor Green = "Go"