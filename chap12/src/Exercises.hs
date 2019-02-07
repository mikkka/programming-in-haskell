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
    fmap f pfa = PF(f. (pfapp pfa))

  instance Applicative (PartFun s) where
    -- a -> PartFun(s -> a)
    pure a = PF(const a)
    -- PartFun(s -> a -> b) -> PartFun(s -> a) -> PartFun(s -> b)
    (<*>) fsab fa = PF(\s -> (pfapp fsab) s ((pfapp fa) s))
