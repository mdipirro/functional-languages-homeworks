import Parser

data LKC =  VAR String | NUM Int | NULL | ADD LKC LKC |
            SUB LKC LKC | MULT LKC LKC | DIV LKC LKC |
            EQ LKC LKC | LEQ LKC LKC | H LKC | T LKC | CONS LKC LKC |
            IF LKC LKC LKC | LAMBDA [LKC] LKC | CALL LKC [LKC] |
            LET LKC [(LKC,LKC)] | LETREC LKC [(LKC, LKC)]
            deriving(Show, Eq)

prog :: Parser LKC
prog = letp <|> letrec

letp :: Parser LKC
letp = do symbol "let"
          b <- bind
          symbol "in"
          e <- expr
          symbol "end"
          return $ LET e b

letrec :: Parser LKC
letrec = do symbol "letrec"
            b <- bind
            symbol "in"
            e <- expr
            symbol "end"
            return $ LETREC e b

bind :: Parser [(LKC, LKC)]
bind =  do  v <- var
            symbol "="
            e <- expr
            do  symbol "and"
                b <- bind
                return $ (v, e) : b
                <|> return [(v, e)]

var :: Parser LKC
var = do  l  <- lower
          ls <- many alphanum
          return $ VAR $ l:ls

expr :: Parser LKC
expr =  prog
        <|> lambda
        <|> expa
-- <|> opp
        <|> ifp

lambda :: Parser LKC
lambda = do symbol "lambda"
            symbol "("
            vs <- seqvar
            symbol ")"
            e <- expr
            return $ LAMBDA vs e

ifp :: Parser LKC
ifp = do  symbol "if"
          g <- expr
          symbol "then"
          t <- expr
          symbol "else"
          f <- expr
          return $ IF g t f

seqvar :: Parser [LKC]
seqvar = do v   <- var
            vs  <- many var
            return $ v:vs

{-|opp :: Parser LKC
opp = do  symbol "cons"
          vs <- seqvar
          return $ CONS vs
-}

expa :: Parser LKC
expa = do t <- term
          do  symbol "+"
              e <- expa
              return $ ADD t e
              <|>
              do  symbol "-"
                  e <- expa
                  return $ SUB t e
                  <|> return t

term :: Parser LKC
term = do f <- factor
          do  symbol "*"
              t <- term
              return $ MULT f t
              <|>
              do  symbol "/"
                  t <- term
                  return $ DIV f t
                  <|> return f

factor :: Parser LKC
factor =  do  v <- var
              do  symbol "Y"
                  y <- functioncall
                  return $ CALL v y
                  <|> return v
          <|>
          do  i <- integer
              return $ NUM i
          <|>
          do  symbol "null"
              return NULL
          <|>
          do  symbol "("
              e <- expa
              symbol ")"
              return e

functioncall :: Parser [LKC]
functioncall = do symbol "("
                  es <- seqexpr
                  symbol ")"
                  return es

seqexpr :: Parser [LKC]
seqexpr = do  e <- expr
              do  symbol ","
                  es <- seqexpr
                  return $ e:es
                  <|> return [e]
