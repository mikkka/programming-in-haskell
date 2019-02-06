module Exercises where
  data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

  instance Functor Tree where
    fmap g Leaf = Leaf
    fmap g (Node l m r) = Node (fmap g l) (g m) (fmap g r)

  class Functar f where
    ffmap :: (a -> b) -> f a -> f b

  class Aapplicative f where
    pura :: a -> f a
    (<~>) :: f (a -> b) -> f a -> f b


  -- bljad!!!!
  
  instance Functar ((->)a) where
    ffmap f x = f . x

  instance Aapplicative ((->)s) where
    pura = const
    -- (s -> a -> b) -> (s -> a) -> (s -> b)
    (<~>) fsab fa = \s -> fsab s (fa s)
    --(<~>) fsab fa s = fsab s (fa s)
