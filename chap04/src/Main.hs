module Main where

main = putStrLn "hello world"

evenn :: Integral a => a -> Bool
evenn n = n `mod` 2 == 0

-- isZeroRemainder :: Integral a => Integral b => a -> b -> Bool
-- isZeroRemainder a b = (a `mod` 3) == (b `mod` 4)


abss n | n >= 0    = n
      | otherwise = negate n

nott :: Bool -> Bool
nott False = True
nott True  = False

(&&&) :: Bool -> Bool -> Bool
True &&& True = True
_    &&& _    = False

startsWith :: [Char] -> Char -> Bool
startsWith (h:_) x  = h == x
startsWith _ _      = False

constt x = \_ -> x

halve :: [a] -> ([a], [a])
halve xs = (take l2 xs, drop l2 xs)
           where
             l2 = (length xs) `div` 2

third1 :: [a] -> a
third1 xs = head (tail (tail xs))

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (_:_:x:_) = x
