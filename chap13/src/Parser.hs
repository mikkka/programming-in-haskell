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

    -- the next 2 are std applicative functions
    -- Parser a -> Parser [a]
    many x = some x <|> pure []
    -- Parser a -> Parser [a]
    some x = pure (:) <*> x <*> many x
    


  sat :: (Char -> Bool) -> Parser Char
  sat p = do 
            x <- item
            if p x then return x else empty

  digit :: Parser Char
  digit = sat isDigit

  lower :: Parser Char
  lower = sat isLower

  upper :: Parser Char
  upper = sat isUpper

  letter :: Parser Char
  letter = sat isAlpha

  alphanum :: Parser Char
  alphanum = letter <|> digit

  char :: Char -> Parser Char
  char x = sat(\c -> c == x)

  string :: String -> Parser String
  string []       = return []
  string (x:xs)   = do 
                      char x
                      string xs
                      return (x:xs)

  ident :: Parser String
  ident = do
            x   <- lower
            xs  <- many alphanum
            return (x:xs)

  nat :: Parser Int
  nat = do
          xs <- some digit
          return (read xs)

  space :: Parser ()
  space = do 
              many (sat isSpace)
              return ()

  int :: Parser Int
  int = do 
          char '-'
          n <- nat
          return (-n)
        <|> nat

  token :: Parser a -> Parser a
  token p = do 
            space
            v <- p
            space
            return v

  identifier :: Parser String
  identifier = token ident

  natural :: Parser Int
  natural = token nat

  integer :: Parser Int
  integer = token int

  symbol :: String -> Parser String
  symbol xs = token (string xs)
