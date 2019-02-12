module Exercises where
  data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

  instance Functor Tree where
    fmap g Leaf = Leaf
    fmap g (Node l m r) = Node (fmap g l) (g m) (fmap g r)

  -- bljad!!!!
  newtype PartFun a b = PF{pfapp :: (a -> b)}
  --pfapp :: PartFun a b -> (a -> b)
  --pfapp (PF f) = f

  instance Functor (PartFun s) where
    --(a -> b) -> PartFun(s -> a) -> PartFun(s -> b)
    fmap f (PF a) = PF(f . a)

  instance Applicative (PartFun s) where
    -- a -> PartFun(s -> a)
    pure a = PF(const a)
    -- PartFun(s -> a -> b) -> PartFun(s -> a) -> PartFun(s -> b)
    (<*>) (PF sab) (PF a) = PF(\s -> sab s (a s))

  instance Monad (PartFun s) where
    -- PartFun(s -> a) -> (a -> PartFun(s -> b)) -> PartFun(s -> b)
    (>>=) (PF a) amb = PF(\s -> (pfapp (amb (a s)) s))

  
  newtype ZipList a = Z [a] deriving Show
  instance Functor ZipList where
    -- (a -> b) -> ZipList a -> ZipList b
    fmap f (Z xs)     = Z (fmap f xs)

  instance Applicative ZipList where
    pure x = Z (repeat x)

    (Z fs) <*> (Z xs) = Z [f x | (f,x) <- zip fs xs]
  
  data Exprr a = Varr a | Vall Int | Addd (Exprr a) (Exprr a)
  instance Functor Exprr where
    -- (a -> b) -> Expr
    fmap f (Varr a)     = Varr (f a)
    fmap f (Vall x)     = Vall x
    fmap f (Addd l r)   = Addd (fmap f l) (fmap f r)

  instance Applicative Exprr where
    pure = Varr
  
    -- Exprr(a -> b) -> Exprr(a) -> Exprr(b)
    -- exf <*> exa = ???
    (Varr f) <*> e      = fmap f e
    (Addd fl fr) <*> e  = Addd (fl <*> e) (fr <*> e)
    (Vall x) <*> _      = Vall x

  instance Monad Exprr where
    --(>>=) :: ma -> (a -> mb) -> mb
    
    (Varr a)    >>= f = f a
    (Addd l r)  >>= f = Addd (l >>= f) (r >>= f)
    (Vall x)    >>= f = Vall x