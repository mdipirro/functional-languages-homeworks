-- Exercise 2
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
              deriving Show

instance Functor Expr where
  --fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var a)      = Var (f a)
  fmap _ (Val i)      = Val i
  fmap f (Add e1 e2)  = Add (fmap f e1) (fmap f e2)

instance Applicative Expr where
  -- pure :: a -> Expr a
  pure = Var

  -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  (Val i) <*> e     = Val i
  (Var f) <*> e     = fmap f e
  (Add f1 f2) <*> e = Add (f1 <*> e) (f2 <*> e)

instance Monad Expr where
  -- return :: a -> Expr a
  return = pure

  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  (Val i) >>= _     = Val i
  (Var a) >>= f     = f a
  (Add e1 e2) >>= f = Add (e1 >>= f) (e2 >>= f)
