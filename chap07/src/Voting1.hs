module Voting1 where

  import Data.List

  count :: Eq a => a -> [a] -> Int
  count x = length . filter (== x)

  rmdups :: Eq a => [a] -> [a]
  rmdups [] = []
  rmdups (x:xs) = x : filter (/= x) (rmdups xs)

  result :: Ord a => [a] -> [(Int,a)]
  result xs = sort [(count x xs, x) | x <- rmdups xs]

  winner :: Ord a => [a] -> a
  winner = snd . last . result