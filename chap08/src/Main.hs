module Main where
  import AMachine
  import Tautology
  import Exercises

  main :: IO ()
  main = putStrLn "hello chapter08"

  data Move = North | South | East | West deriving Show

  data Nat = Zero | Succ Nat

  add :: Nat -> Nat -> Nat
  add Zero n      = n
  add (Succ m) n  = Succ (add m n)   

  data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)