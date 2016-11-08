import System.IO

main = hangman

sgetChar :: IO Char
sgetChar = do hSetEcho stdin False
              x <- getChar
              hSetEcho stdin True
              return x

sgetLine :: IO String
sgetLine = do x <- sgetChar
              if x == '\n'
              then do putChar x
                      return []
              else do putChar '-'
                      xs <- sgetLine
                      return (x:xs)

hangman :: IO ()
hangman = do  putStrLn "Think a word"
              word <- sgetLine
              putStrLn "Try to guess it:"
              play word

play :: String -> IO ()
play word = do  putStr "? "
                guess <- getLine -- read the guess
                if guess == word
                then putStrLn "You got it!" -- if guessed, compliments!
                else do putStrLn (map (\c ->  if elem c guess
                                              then c
                                              else '-') word)
                        play word -- otherwise show the guessed chars and let the user retries!

{-|
This `play'` function is a more sophisticated version of the `play` one.
Given a charachter `c`, if `c` is an element of guess, the latters shows every
occurence of `c` in `word`. Indeed, if in `guess` there is only one occurrence
of, let's say, 'a', and in `word` there are three occurences of 'a', `play`
shows three occurences. `play'`, instead, shows only the first occurrence of
'a'. More formally, given a character `c`, `play'` shows only the first `n`
occurrences of `c` in `word`, where `n` is the number of the occurrences of `c`
in `guess`.
-}
play' :: String -> IO ()
play' word = do  putStr "? "
                 guess <- getLine
                 if word == guess
                 then putStrLn "You got it!"
                 else do putStrLn (discover word guess)
                         play' word
             where discover [] _                        = ""
                   discover (c:cs) guess | elem c guess = c : discover cs (deleteFirst c guess)
                                         | otherwise    = '-' : discover cs guess
                   deleteFirst _ []                  = []
                   deleteFirst a (b:bs)  | a == b    = bs
                                         | otherwise = b : deleteFirst a bs
