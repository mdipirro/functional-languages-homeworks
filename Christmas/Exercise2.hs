import Parser

data Expr = Add Expr Expr | Sub Expr Expr | Term Expr |
            Mult Expr Expr | Div Expr Expr | Factor Expr |
            Nat Int
            deriving Show

{-|
The Expr type represents the following grammar:

expr ::= term + expr | term - expr | term
term ::= factor * term | factor / term | factor
factor ::= (expr) | nat
nat ::= 0 | 1 | 2
-}

factor :: Parser Expr
factor =  do  symbol "("
              e <- expr
              symbol ")"
              return e
          <|> do  n <- natural
                  return $ Factor $ Nat n


term :: Parser Expr
term = do f <- factor
          do  symbol "*"
              t <- term
              return (Mult f t)
              <|>
              do  symbol "/"
                  t <- term
                  return (Div f t)
                  <|> return (Term f)


expr :: Parser Expr
expr = do t <- term
          do  symbol "+"
              e <- expr
              return $ Add t e
              <|>
              do  symbol "-"
                  e <- expr
                  return $ Sub t e
                  <|> return t
