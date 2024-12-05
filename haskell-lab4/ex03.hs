data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt



data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a) 

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"


depthOfBT :: BinTree a -> Int -- głębokość drzewa binarnego
flattenBT :: BinTree a -> [a]  -- preorder
flattenBT' :: BinTree a -> [a] --inorder
flattenBT'' :: BinTree a -> [a] --postorder
mapBT :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego
insert :: Ord a => a -> BinTree a -> BinTree a -- insert element into BinTree
list2BST :: Ord a => [a] -> BinTree a -- list to Binary Search Tree (BST)

depthOfBT EmptyBT = 0
depthOfBT (NodeBT _ left right) = 1 + max (depthOfBT left) (depthOfBT right)

flattenBT EmptyBT = []
flattenBT (NodeBT x left right) = [x] ++ flattenBT left ++ flattenBT right

flattenBT' EmptyBT = []
flattenBT' (NodeBT x left right) = flattenBT left ++ [x] ++ flattenBT right

flattenBT'' EmptyBT = []
flattenBT'' (NodeBT x left right) = flattenBT left ++ flattenBT right ++ [x]

mapBT _ EmptyBT = EmptyBT
mapBT f (NodeBT x left right) = NodeBT (f x) (mapBT f left) (mapBT f right)

insert newVal EmptyBT = NodeBT newVal EmptyBT EmptyBT
insert newVal (NodeBT x left right) 
    | newVal == x = NodeBT x left right
    | newVal < x = NodeBT x (insert newVal left) right
    | newVal > x = NodeBT x left (insert newVal right)

list2BST [] = EmptyBT
list2BST (x:xs) = insert x (list2BST xs)

----------------------------
-- occurs :: Eq a => a -> BinTree a -> Int -- liczba wystąpień elementu w drzewie binarnym
-- elemOf :: Eq a => a -> BinTree a -> Bool -- sprawdzenie, czy element znajduje się w drzewie
-- reflect :: BinTree a -> BinTree a -- 'odbicie lustrzane' drzewa binarnego
-- minElemOf :: Ord a => BinTree a -> a
-- maxElemOf :: Ord a => BinTree a -> a
-- foldBinTree :: (a -> b -> b -> b) -> b -> BinTree a -> b -- fold dla drzewa binarnego

data GTree a = Leaf a |
               GNode [GTree a]
               deriving Show

-- sumGTree :: Num a => GTree a -> a
-- elemOfGTree :: Eq a => a -> GTree a -> Bool
-- depthOfGTree :: GTree a -> Int
-- mapGTree :: (a -> b) -> GTree a -> GTree b
-- flattenGTree :: GTree a -> [a]
-- countGTreeLeaves :: GTree a -> Int


data Expr' a = Lit' a |
            Expr a :+: Expr a |
            Expr a :-: Expr a |
            Expr a :*: Expr a

eval' :: Num a => Expr' a -> a
eval' (Lit' n) = n
eval' ( e1 :+: e2) = eval e1 + eval e2
eval' (e1 :-: e2) = eval e1 - eval e2
eval' (e1 :*: e2) = eval e1 * eval e2 

show'' :: Show a => Expr' a -> String
show'' (Lit' n) = show n
show'' (e1 :+: e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show'' (e1 :-: e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
show'' (e1 :*: e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"