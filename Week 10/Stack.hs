type Stack = [Char]

-- First solution
pop :: Stack -> (Char, Stack)
pop (x:xs) = (x, xs)

push :: Char -> Stack -> ((), Stack)
push c xs = ((), c:xs)

check_par :: [Char] -> Stack -> Bool
check_par [] xs     = null xs
check_par (c:cs) xs | c == '('  = let (_, xs') = push ')' xs
                                  in check_par cs xs'
                    | c == ')'  = let (_, xs') = pop xs
                                  in check_par cs xs'
                    | otherwise = check_par cs xs

-- Second solution
type State    = Stack
newtype ST a  = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

info :: ST a -> State -> a
info st x = (\(a, s) -> a) (app st x)

instance Functor ST where
  fmap g st = S (\s ->  let (x, s') = app st s
                        in (g x, s'))

instance Applicative ST where
  pure x      = S (\s -> (x, s))
  stf <*> stx = S (\s ->  let (f, s')   = app stf s
                              (x, s'')  = app stx s'
                              in (f x, s''))

instance Monad ST where
  return    = pure
  st >>= f  = S (\s ->  let (x, s') = app st s
                        in app (f x) s')

-- Checks if the Stack is empty or not
empty :: ST Bool
empty = S (\xs -> (null xs, xs))

-- Pops the first element
pop1 :: ST Char
pop1 = S (\(x:xs) -> (x, xs))

-- Push an element at the top of the stack
push1 :: ST ()
push1 = S (\x -> ((), ')':x))

-- Checks whether or not the parenthesis are balanced
check_par1 :: [Char] -> ST Bool
check_par1 []       = empty >>= \e  -> return e
check_par1 ('(':cs) = push1 >>= \_  -> check_par1 cs
check_par1 (')':cs) = pop1  >>= \_  -> check_par1 cs
check_par1 (c:cs)   = check_par1 cs
{-|
With DO notation
check_par1 []       = do  e <- empty
                          return e
check_par1 ('(':cs) = do  _ <- push1
                          check_par1 cs
check_par1 (')':cs) = do  _ <- pop1
                          check_par1 cs
check_par1 (c:cs)   = check_par1 cs
-}
