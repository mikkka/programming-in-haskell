module Main where

import Data.Char
import Exrcs

main = putStrLn "hello chap05"

compre = [(x, y) | 
            x <- [1..3], 
            y <- [x..3]
          ]

concatt :: [[a]] -> [a]
concatt xss = [x | xs <- xss, x <- xs] 

factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], n `mod` x == 0]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]


isLowerr :: Char -> Bool
isLowerr c = ord c >= ord 'a' && ord c <= ord 'z'

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLowerr c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (countPred (\c -> c == x) xs) n | x <- ['a'..'z']]
              where n = length (filter isLowerr xs)

countPred :: (a -> Bool) -> [a] -> Int
countPred pred xs = length (filter pred xs)

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..n], x == x']
                  where n = length xs - 1

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
          0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
          6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]