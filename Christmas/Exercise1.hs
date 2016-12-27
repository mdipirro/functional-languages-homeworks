import Parser

-- Exercise 1
comment :: Parser ()
comment = do  symbol "--"
              many (sat (/='\n'))
              char '\n'
              return ()
-- Exercise 2
