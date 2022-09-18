
--1
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

data List a = Empty | Cons a (List a) deriving Show


listZip :: List a -> List b -> List (a,b)
listZip Empty ys = Empty
listZip _ Empty = Empty
listZip (Cons x xs) (Cons y ys) = Cons(x, y) (listZip xs ys)

--2

data BTree t = EmptyTree | Node t  (BTree t) (BTree t) deriving (Show)

insert :: Ord a => a -> BTree a -> BTree a
insert elem EmptyTree = Node elem EmptyTree EmptyTree
insert v (Node elem left right)
  | v < elem = Node elem (insert v left) right
  | v > elem = Node elem left (insert v right )

--3
data Nat = Zero | Succ Nat deriving (Show)

natPlus :: Nat -> Nat -> Nat
natPlus Zero x = x
natPlus (Succ a) b = natPlus a (Succ b)

natMult :: Nat -> Nat -> Nat
natMult x Zero = Zero
natMult Zero x = Zero
natMult (Succ x) y = natPlus (natMult x y) y

--4

instance Eq t => Eq (BTree t) where
  EmptyTree == EmptyTree = True
  EmptyTree == _ = False
  _ == EmptyTree = False
  (Node x lx rx) == (Node y ly ry) = x == y && lx == ly && rx == ry

--5 
data AssocList k v = ALEmpty | ALCons k v (AssocList k v) deriving (Show)

instance Functor (AssocList k) where
  fmap _ ALEmpty = ALEmpty
  fmap f (ALCons k v rest)  = ALCons k (f v) (fmap f rest)
  
  











    