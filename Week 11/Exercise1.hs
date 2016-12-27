-- Exercise 1
instance Monad ((->)a) where
  -- return	::	b	->	(a -> b)
  return x = (\_ -> x)

  -- (>>=) :: (a -> b) -> (b -> (a -> c)) -> (a -> c)
  g >>= h = (\x -> h $ g x)
