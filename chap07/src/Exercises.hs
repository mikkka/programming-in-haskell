module Exercises where

forcomp :: [a] -> (a -> b) -> (a -> Bool) -> [b]
forcomp xs f p = map f (filter p xs)

all :: (a -> Bool) -> [a] -> Bool
all p xs = and (map p xs)

any :: (a -> Bool) -> [a] -> Bool
any p xs = or (map p xs)

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p []      = []
takeWhile1 p (x:xs)  = if p x 
                        then x : takeWhile1 p xs
                        else []

dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 p []      = []
dropWhile1 p (x:xs)  = if p x 
                        then dropWhile1 p xs
                        else xs
                        
mapp :: (a -> b) -> [a] -> [b]
mapp f = foldr (\x y -> f x : y) []

filterr :: (a -> Bool) -> [a] -> [a]
filterr p = foldr (\x y -> if p x then (x : y) else y) []

dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> acc * 10 + x) 0

carry :: ((a,b) -> c) -> (a -> b -> c)
carry f x y = f (x, y)

uncarry :: (a -> b -> c) -> ((a,b) -> c)
uncarry f xy = f (fst xy) (snd xy)

unfold p h t x  | p x = []
                | otherwise = h x : unfold p h t (t x)

mapU :: (a -> b) -> [a] -> [b]
mapU f = unfold null (f . head) tail

iterateU :: (a -> a) -> a -> [a]
iterateU = unfold (const False) id
