module Main where

import BinStrTrans
import Voting1
import Voting2
import Exercises

main = putStrLn "hello chap07"

lmbda = \x -> \y -> x + y

plusFL :: (Foldable t, Num b) => b -> t b -> b 
plusFL = foldl (+)

twice f = f . f 