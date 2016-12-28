import Parser

data Expr = Add Expr Expr | Sub Expr Expr | Term Expr |
            Mult Expr Expr | Div Expr Expr | Factor Expr |
            Par Expr | Nat Int
            deriving Show

{-|
The Expr tyoe represets the following grammar:

expr ::= term + expr | term - expr | term
term ::= factor * term | factor / term | factor
factor ::= (expr) | nat
nat ::= 0 | 1 | 2
-}

factor :: Parser Expr
factor =  do  symbol "("
              e <- expr
              symbol ")"
              return (Par e)
          <|> do  n <- natural
                  return (Nat n)


term :: Parser Expr
term = do f <- factor
          do  symbol "*"
              t <- term
              return (Mult (Factor f) (Term t))
              <|>
              do  symbol "/"
                  t <- term
                  return (Div (Factor f) (Term t))
                  <|> return (Factor f)


expr :: Parser Expr
expr = do t <- term
          do  symbol "+"
              e <- expr
              return (Add (Term t) e)
              <|>
              do  symbol "-"
                  e <- expr
                  return (Sub (Term t) e)
                  <|> return (Term t)
