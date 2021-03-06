module ArithmeticExpParser where
  import Control.Applicative
  import Parser


  -- expr ::= term ( + expr | - expr | E)

  expr :: Parser Int
  expr = 
    term >>= \t -> 
              (
                symbol "+" >> 
                expr >>= \e -> return (t + e)
              )  
              <|> 
              (
                symbol "-" >> 
                expr >>= \e -> return (t - e)
              )  
              <|> return t
      -- do 
      --   t <- term
      --   do 
      --     symbol "+"
      --     e <- expr
      --     return (t + e)
      --     <|> return t

-- term ::= factor ( * term | / term | E)

  term :: Parser Int
  term =  
    factor >>= \f -> 
                (
                  symbol "*" >> 
                  term >>= \t -> return (f * t)
                ) 
                <|>
                (
                  symbol "/" >> 
                  term >>= \t -> return (f `div` t)
                ) 
                <|> return f

      -- do 
      --   f <- factor
      --   do 
      --     symbol "*"
      --     t <- term
      --     return (f * t)
      --     <|> pure f


-- factor ::= ( expr ) | nat

  factor :: Parser Int
  factor =  (
              symbol "(" >> 
              expr >>= \e -> symbol ")" >> return e
            )
            <|> natural


      -- do
      --   symbol "("
      --   e <- expr
      --   symbol ")"
      --   return e
      --   <|> natural

