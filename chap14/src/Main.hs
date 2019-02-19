module Main where
  import Data.Monoid
  import Data.Foldable

  main :: IO ()
  main = putStrLn "hello chap14"

  data Tree a = Leaf a | Node (Tree a) (Tree a)

  instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Leaf a)     = Leaf (f a)
    fmap f (Node l r)   = Node (fmap f l) (fmap f r)
  
  instance Foldable Tree where
    -- fold :: Monoid a => Tree a -> a
    fold (Leaf x) = x
    fold (Node l r) = fold l `mappend` fold r
 
    -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
    foldMap f (Leaf x) = f x
    foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

    -- foldr  :: (a -> b -> b) -> b -> Tree a -> b
    foldr f v (Leaf x) = f x v
    foldr f v (Node l r) = foldr f (foldr f v r) l

    -- foldl  :: (a -> b -> a) -> a -> Tree b -> a
    foldl f v (Leaf x) = f v x
    foldl f v (Node l r) = foldl f (foldl f v l) r


  instance Traversable Tree where
    -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse g (Leaf x)     = pure Leaf <*> g x
    traverse g (Node l r)   = pure Node <*> traverse g l <*> traverse g r


  -- data Pair a b = P a b
  newtype Pair a b = Pair (a, b)

  instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
    (<>) (Pair (x1, y1)) (Pair (x2, y2)) = Pair (x1 <> x2, y1 <> y2)

  instance (Monoid a, Monoid b) => Monoid (Pair a b) where
    -- mempty :: (Pair a b)
    mempty = Pair (mempty, mempty)

    -- mappend :: (Pair a b) -> (Pair a b) -> (Pair a b) -- defined in semigroup
    -- (P x1 y1) `mappend`(P x2 y2) = (P (x1 `mappend` x2) (y1 `mappend` y2))

  newtype Funky a b = Funky (a -> b)

  instance Semigroup b => Semigroup (Funky a b) where
    (<>) (Funky f1) (Funky f2) = Funky (\s -> f1 s <> f2 s)

  instance Monoid b => Monoid (Funky a b) where
    mempty = Funky (const mempty)
  