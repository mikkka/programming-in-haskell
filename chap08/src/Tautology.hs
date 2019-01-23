module Tautology where

  rmdups :: Eq a => [a] -> [a]
  rmdups [] = []
  rmdups (x:xs) = x : filter (/= x) (rmdups xs)

  type Assoc k v = [(k, v)]

  find :: Eq k => k -> Assoc k v -> v
  find k (x:xs)  
                | fst x == k = snd x
                | otherwise = find k xs

  type Subst = Assoc Char Bool

  data Prop = Const Bool
              | Var Char
              | Not Prop
              | And Prop Prop
              | Imply Prop Prop

  p1 :: Prop
  p1 = And (Var 'A') (Not (Var 'A'))

  eval :: Subst -> Prop -> Bool
  eval _ (Const b)    = b
  eval s (Var v)      = find v s
  eval s (Not p)      = not (eval s p)
  eval s (And l r)    = (eval s l) && (eval s r)
  eval s (Imply l r)  = (eval s l) <= (eval s r)

  vars :: Prop -> [Char]
  vars (Const _)    = []
  vars (Var v)      = [v]
  vars (Not p)      = vars p
  vats (And l r)    = vars l ++ vars r
  vats (Imply l r)  = vars l ++ vars r

  bools :: Int -> [[Bool]]
  bools 0     = [[]] 
  bools n     = map (False:) bss ++ map (True:) bss
                where
                  bss = bools (n - 1)


  substs :: Prop -> [Subst]
  substs p = map (zip vs) (bools (length vs))
             where vs = vars p


  isTaut :: Prop -> Bool
  isTaut p = and [eval s p | s <- substs p]
