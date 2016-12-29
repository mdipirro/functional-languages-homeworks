import Parser

data LKC =  VAR String | NUM Int | NULL | ADD LKC LKC |
            SUB LKC LKC | MULT LKC LKC | DIV LKC LKC |
            EQ LKC LKC | LEQ LKC LKC | H LKC | T LKC | CONS LKC LKC |
            IF LKC LKC LKC | LAMBDA [LKC] LKC | CALL LKC [LKC] |
            LET LKC [(LKC,LKC)] | LETREC LKC [(LKC, LKC)]
            deriving(Show, Eq)

keywords :: [String]
keywords = ["cons", "head", "tail", "eq", "leq", "lambda", "if", "let", "null",
  "letrec"]

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
          let v = l:ls
          if not $ elem v keywords
          then return $ VAR v
          else empty

expr :: Parser LKC
expr =  prog <|> lambda <|> expa <|> opp <|> ifp

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

opp :: Parser LKC
opp = cons <|> headp <|> tailp <|> eq <|> leq

cons :: Parser LKC
cons = do symbol "cons"
          (e1, e2) <- twoarg
          return $ CONS e1 e2

headp :: Parser LKC
headp = do  symbol "head"
            e <- onearg
            return $ H e

tailp :: Parser LKC
tailp = do  symbol "tail"
            e <- onearg
            return $ T e

eq :: Parser LKC
eq = do symbol "eq"
        (e1, e2) <- twoarg
        return $ Main.EQ e1 e2

leq :: Parser LKC
leq = do  symbol "leq"
          (e1, e2) <- twoarg
          return $ LEQ e1 e2

onearg :: Parser LKC
onearg = do symbol "("
            e <- expr
            symbol ")"
            return e


twoarg :: Parser (LKC, LKC)
twoarg = do symbol "("
            e1 <- expr
            symbol ","
            e2 <- expr
            symbol ")"
            return (e1, e2)

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
              do  y <- functioncall
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
seqexpr = do  e   <- expr
              es  <- many (do symbol ","
                              expr)
              return $ e:es


{-|do  symbol ","
    es <- seqexpr
    return $ e:es
    <|> return [e]
-}
