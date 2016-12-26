--          ST Monad
type State    = Int
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

fresh :: [Int] -> ST [Int]
fresh xs = S (\s -> (map (+s) xs, s + 1))

increment :: [[Int]] -> ST [[Int]]
increment []        = pure []
increment (xs:xss)  = pure (:) <*> fresh xs <*> increment xss
