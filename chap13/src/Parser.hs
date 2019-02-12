module Parser where
  
  import Control.Applicative
  import Data.Char

  newtype Parser a = P (String -> [(a, String)])
  
  parse :: Parser a -> String -> [(a, String)]
  parse (P p) = p

  item :: Parser Char
  item = P (\inp -> case inp of
                        []      -> []
                        (x:xs)  -> [(x, xs)])

  instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P(\inp -> case parse p inp of
                    []            -> []
                    [(a, rest)]   -> [(f a, rest)]
                )

  instance Applicative Parser where
    --pure :: a -> Parser a
    pure a = P(\inp -> [(a, inp)])

    -- <*> :: Parser(a -> b) -> Parser a -> Parser b
    pab <*> pa = P(\inp -> case parse pab inp of
                    []            -> []
                    [(ab, out)]   -> parse (fmap ab pa) out
                  )

  -- example of applicative use
  three :: Parser (Char, Char)
  three = pure fun <*> item <*> item <*> item
          where fun x y z = (x, z)

  
  instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= pab = P(\inp -> case parse pa inp of
                    []            -> []
                    [(a, out)]    -> parse (pab a) out
                  )

  threeM :: Parser (Char, Char)
  threeM = do
              x <- item
              item
              z <- item
              return (x, z)

  instance Alternative Parser where
    --empty :: Parser a
    empty = P(const []) -- always return empty parse result []
    
    -- <|> :: Parser a -> Parser a -> Parser a
    p <|> q = P(\inp -> case parse p inp of
                  []              -> parse q inp -- q is better cause non empty
                  x               -> x           -- p is better cause first
              )
    