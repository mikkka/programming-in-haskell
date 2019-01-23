module Main where
import Hangman
import Life

main :: IO ()
main = do
  putStrLn "hello chap10"


getLinn :: IO String
getLinn = do x <- getChar
             if x == '\n' then
               return []
             else
               do xs <- getLinn
                  return (x:xs)
      
      
putStrr :: String -> IO ()
putStrr []      = return ()
putStrr (x:xs)  = do putChar x
                     putStrr xs



putStrLnn :: String -> IO ()
putStrLnn xs = do putStrr  xs
                  putChar '\n'


