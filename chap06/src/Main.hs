module Main where

  main = putStrLn "chap06"


  fac :: Int -> Int
  fac 0 = 1
  fac n = n * fac (n - 1)

  productt :: Num a => [a] -> a
  productt []       = 0
  productt [n]      = n
  productt (n:ns)   = n * productt ns

  insert :: Ord a => a -> [a] -> [a]
  insert x []                   = [x]
  insert x (y:ys) | x <= y      = x : y : ys
                  | otherwise   = y : insert x ys


  zipp :: [a] -> [b] -> [(a,b)]
  zipp [] _          = []
  zipp _ []          = []
  zipp (x:xs) (y:ys) = (x,y) : zipp xs ys


  merge :: Ord a => [a] -> [a] -> [a]
  merge [] x                        = x
  merge x []                        = x
  merge (x:xs) (y:ys) | x < y       = x : merge xs (y:ys)
                      | otherwise   = y : merge (x:xs) ys

  halve :: [a] -> ([a],[a])
  halve []        = ([],[])
  halve [x]       = ([x],[])
  halve [x,y]     = ([x],[y])
  halve (x:y:xs)  = (x:lft, y:rgt)
                    where
                      (lft,rgt) = halve xs


  msort :: Ord a => [a] -> [a]
  msort []        = []
  msort [x]       = [x]
  msort xs        = merge lft rgt
                    where
                      (lft',rgt') = halve xs
                      lft = msort lft'
                      rgt = msort rgt'


  euclid :: Int -> Int -> Int
  euclid x y | x == y       = x
             | x <= y       = euclid x (y - x)
             | x > y        = euclid (x - y) y
