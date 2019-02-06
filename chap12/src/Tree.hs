module Tree where
  import State

  data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

  instance Functor Tree where 
    fmap g (Leaf a) = Leaf (g a)
    fmap g (Node l r) = (Node (fmap g l) (fmap g r))
  
  tree :: Tree Char
  tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

  rlabel :: Tree a -> Int -> (Tree Int, Int)
  rlabel (Leaf _) n   = (Leaf n, n + 1)
  rlabel (Node l r) n = (Node l' r', n'')
                        where
                          (l', n')  = rlabel l n
                          (r', n'') = rlabel r n'

  fresh :: ST Int
  fresh = S(\n -> (n, n + 1))

  alabel :: Tree a -> ST (Tree Int)
  -- pure (a -> Tree a) <*> ST Int -> ST (Tree Int)
  --alabel (Leaf _)   = pure Leaf <*> fresh
  alabel (Leaf _)   = Leaf <$> fresh

  -- pure (Tree a -> Tree a -> Tree a) <*> ST (Tree Int) <*> ST (Tree Int) -> ST (Tree Int)
  --alabel (Node l r) = pure Node <*> alabel l <*> alabel r
  alabel (Node l r) = Node <$> alabel l <*> alabel r


  mlabel :: Tree a -> ST (Tree Int)
  mlabel (Leaf _) = do 
                      n <- fresh
                      return (Leaf n)
  
  mlabel (Node l r) = do 
                        l' <- mlabel l
                        r' <- mlabel r
                        return (Node l' r')
