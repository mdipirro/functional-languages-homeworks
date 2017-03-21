import Parser

{-|
The grammar would be:

expr ::= expr - nat | nat
nat ::= 0 | 1 | 2 ...
-}

expr :: Parser Int
expr = do e <- Main.expr
          do  symbol "-"
              n <- nat
              return (e - n)
              <|> return e

--The parser loops forever because of its left recursivity.

expr' :: Parser Int
expr' = do  n <- nat
            ns <- many (do  symbol "-"
                            nat)
            return (foldl (-) n ns)
