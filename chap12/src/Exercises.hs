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
