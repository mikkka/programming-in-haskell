module Main where

main :: IO ()
main = 
  putStrLn "hello world"


-- sumFun :: Num a => [a] -> a
sumFun [] = 0
sumFun(n:ns) = n + sum ns

-- qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = 
              qsort smaller ++ [x] ++ qsort larger
                where
                  smaller = [a | a <- xs, a <= x]
                  larger  = [b | b <- xs, b > x]

double x = x + x
quadruple x = double (double  x)
average ns = sum ns `div` length ns

n = a `div` length xs
                  where
                    a = 10
                    xs = [1,2,3,4,5]