module Parser (Parser(P), parse, item, sat, digit, char, string, nat, space, token,
symbol, expr, factor, term, pure, fmap, (<*>), (>>=), empty, (<|>), many, some,
solve) where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

instance Functor Parser where
  fmap g p = P (\inp -> case parse p inp of [] -> []
                                            [(v, out)] -> [(g v, out)])

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])
  pg <*> px = P (\inp -> case parse pg inp of [] -> []
                                              [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
  p >>= f = P (\inp -> case parse p inp of  [] -> []
                                            [(v, out)] -> parse (f v) out)

instance Alternative Parser where
  empty = P (\inp -> [])
  p <|> q = P (\inp -> case parse p inp of  [] -> parse q inp
                                            [(v, out)] -> [(v, out)])

item :: Parser Char
item = P (\inp -> case inp of [] -> []
                              (x:xs) -> [(x,xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do  x <- item
            if p x
            then return x
            else empty

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do  char x
                    string xs
                    return (x:xs)

nat :: Parser Int
nat = do  xs <- some digit
          return (read xs)

space :: Parser ()
space = do  many (sat isSpace)
            return ()

token :: Parser a -> Parser a
token p = do  space
              v <- p
              space
              return v

symbol :: String -> Parser String
symbol xs = token (string xs)

factor :: Parser Int
factor =  do  symbol "("
              e <- expr
              symbol ")"
              return e
          <|> nat

term :: Parser Int
term = do f <- factor
          do  symbol "*"
              t <- term
              return (f * t)
              <|> return f

expr :: Parser Int
expr = do t <- term
          do  symbol "+"
              e <- expr
              return (t + e)
              <|> return t

solve :: String -> Int
solve e = (\[(solution, other)] -> solution) (parse expr  e)
