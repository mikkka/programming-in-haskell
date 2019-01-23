module Exercises where
  data Expr = Val Int | Add Expr Expr

  folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
  folde f g (Val x)         = f x
  folde f g (Add lft rgt)   = g (folde f g lft) (folde f g rgt)

  eval :: Expr -> Int
  eval = folde id (+)

  size :: Expr -> Int
  size = folde (const 1) (+)

  data Option a = None | Some a deriving (Show)

  instance Eq a => Eq (Option a) where
    (Some a) == (Some b)  = a == b
    None == None          = True
    _ == _                = False

  
  data List a = Nil | Cons a (List a) deriving Show
  
  instance Eq a => Eq (List a) where
    Nil         == Nil            = True
    (Cons x xs) == (Cons y ys)    = x == y && xs == ys
    _           == _              = False