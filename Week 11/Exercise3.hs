-- Exercise 3
type Stack = [Char]
type State    = Stack
newtype ST a  = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Monad ST where
  st >>= f = S(\s ->  let (x,s') = app st s
                      in app (f x) s')

instance Functor ST where
  fmap g st = do  x <- st
                  return (g x)

instance Applicative ST where
  pure x      = S (\s -> (x, s))
  stf <*> stx = do  f <- stf
                    x <- stx
                    return (f x)
