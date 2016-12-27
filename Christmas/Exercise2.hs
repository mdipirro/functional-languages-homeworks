import Parser

data Expr = Add Expr Expr | Sub Expr Expr |
            Mult Expr Expr | Div Expr Expr |
            Nat Int | Par Expr

{-|
The grammar is
expr ::= term + expr | term - expr
term ::= factor * term | factor / term
factor ::= (expr) | nat
nat ::= 0 | 1 | 2
-}
