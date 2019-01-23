module Main where

import GHC.Exts

main :: IO ()
main = putStrLn "hello chap09"

data Op = Add | Sub | Mul | Div | Exp

ops :: [Op]
ops = [Add,Sub,Mul,Div,Exp]

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /=1 && x <= y
valid Div x y = y > 1 && x `mod` y == 0
valid Exp x y = x > 1 && y > 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val x) = show x
  show (App o l r) = bracketed l ++ show o ++ bracketed r
                      where
                        bracketed (Val x) = show x
                        bracketed e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val x)      = [x]
values (App _ l r)  = values l ++ values r

eval :: Expr -> [Int]
eval (Val x)      = [x | x > 0]
eval (App o l r)  = [apply o x y | x <- eval l,
                                   y <- eval r, 
                                   valid o x y]

subs :: [a] -> [[a]]
subs []           = [[]]
subs (x:xs)       = yss ++ map (x:) yss
                    where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []       = [[x]]
interleave x (y:ys)   = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []          = [[]]
perms (x:xs)      = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices xs = [
  res | sub <- subs xs,
        res <- perms sub]

choices' :: [a] -> [[a]]
choices' = concat . ((map perms) . subs)

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

exprs :: [Int] -> [Expr]
exprs []    = []
exprs [n]   = [Val n]
exprs ns    = [e | (ls, rs)  <- split ns,
                   l         <- exprs ls,
                   r         <- exprs rs,
                   e         <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

type Result = (Expr,Int)

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

results :: [Int] -> [Result]
results []        = []
results [n]       = [(Val n, n) | n > 0]
results ns        = [res | (ls,rs)    <- split ns,
                           lx         <- results ls,
                           ry         <- results rs,
                           res        <- combine' lx ry]

solutions' :: [Int] -> Int -> [Expr]
solutions' = solutions'' (length . values) 0

solutions'' :: Ord a => (Expr -> a) -> Int -> [Int] -> Int -> [Expr]
solutions'' ordF thresh ns n = sortWith ordF [e | ns' <- choices ns, (e,m) <- results ns', m >= n - thresh && m <= n + thresh]

