module Main where

main :: IO ()
main = putStrLn "hello chap 12 - some funcy concepts"

{-
class Functar f where
  fmap :: (a -> b) -> f a -> f b

instance Functar [] where
  fmap = map

instance Functar Maybe where
  fmap _ Nothing = Nothing
  fmap g (Just x) = Just (g x)
-}

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where 
  fmap g (Leaf a) = Leaf (g a)
  fmap g (Node l r) = (Node (fmap g l) (fmap g r))

lst1 :: [Int]
lst1 = pure (*) <*> [1,2,3] <*> [5,6,7]

lst2 :: [Int]
lst2 = (*) <$> [1,2,3] <*> [5,6,7]

data Expr = Val Int | Div Expr Expr

(>>==) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>== f = case mx of
            Nothing -> Nothing
            Just x  -> f x

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

eval :: Expr -> Maybe Int
eval (Val n) = Just n
-- eval (Div l r) = eval l >>== \ll -> 
--                   eval r >>== \rr -> 
--                     safediv ll rr
eval (Div l r) = do 
                  ll <- eval l
                  rr <- eval r
                  safediv ll rr
