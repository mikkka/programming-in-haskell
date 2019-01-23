module Exrcs where
  
sum100 = sum [x^2 | x <- [1..100]]

grid n m = [(y,x) | y <- [0..n], x <- [0..m]]

pyth :: Integer -> [(Integer,Integer,Integer)]
pyth max = [
    (x,y,floor z2Sqrt) | 
    x <- [1..(max - 1)],
    let x2 = x * x,
    y <- [1..(max - 1)],
    let y2 = y * y,
    let z2 = x2 + y2,
    let z2Sqrt = sqrt (fromInteger z2)::Double,
    z2Sqrt == fromIntegral (floor z2Sqrt)
  ]
  where
    max2 = max * max

factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [
    x |
     x <- [1..n],
    (sum (filter (\i -> not (i == x)) (factors x))) == x
  ]