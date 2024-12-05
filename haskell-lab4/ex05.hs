data MyInt = MkMyInt Int
instance Eq MyInt where
  (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2
instance Ord MyInt where
  (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2


instance Num MyInt where
  (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
  (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
  (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
  negate (MkMyInt i)            = MkMyInt (negate i)
  abs (MkMyInt i)               = MkMyInt (abs i)
  signum (MkMyInt i)            = MkMyInt (signum i)
  fromInteger int               = MkMyInt (fromIntegral int)

instance Show MyInt where
  show (MkMyInt i) = "MkMyInt " ++ show i

data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a)

instance Eq a => a => Eq (BinTree a) where
    (==) EmptyBT EmptyBT = True 
    (==) EmptyBT _ = False
    (==) _ EmptyBT = False
    (==) (NodeBT x1 left1 right1) (NodeBT x2 left2 right2) = x1==x2 && left1==left2 && right1==right2
