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
