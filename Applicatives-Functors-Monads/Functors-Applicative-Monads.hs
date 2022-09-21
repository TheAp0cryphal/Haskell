--1
{-# LANGUAGE InstanceSigs #-}

data ErrJst e j = Err e | Jst j deriving (Show)

instance Functor (ErrJst e) where
    fmap :: (a -> b) -> ErrJst e a -> ErrJst e b
    fmap f (Jst j) = Jst (f j)
    fmap f (Err e) = Err e


--2

instance Applicative (ErrJst e) where
  (<*>) :: ErrJst e (a -> b) -> ErrJst e a -> ErrJst e b
  pure = Jst
  Err e <*> _ = Err e
  (Jst f) <*> x = fmap f x

--3 

instance Monad (ErrJst e) where
  (>>=) :: ErrJst e a -> (a -> ErrJst e b) -> ErrJst e b
  Err e >>= _ = Err e
  Jst x >>= f = f x

--4

join :: Monad m => m (m b) -> m b
join m = m >>= id

--5

data LTree a = Leaf a | LNode (LTree a) (LTree a) deriving (Show)

instance Foldable LTree where
  foldMap :: Monoid b => (a -> b) -> LTree a -> b
  foldMap f (Leaf a) = f a
  foldMap f (LNode left right) = foldMap f left `mappend` foldMap f right


