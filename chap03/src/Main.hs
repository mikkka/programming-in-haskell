module Main where

main = putStrLn "chap 03"

tple :: a -> b -> (a, b)
tple x y = (x, y)

barabeq :: Eq a => a -> a -> Bool
barabeq x y = x == y

second xs = head(tail xs)