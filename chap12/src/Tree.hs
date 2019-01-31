module Tree where
  import State

  data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

  tree :: Tree Char
  tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

  fresh :: ST Int
  fresh = S(\n -> (n, n + 1))