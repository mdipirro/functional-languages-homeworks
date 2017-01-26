# pure id <*> x = x
pure id <*> x 

		applying pure
		
= (Just id) <*> x

		applying <*>
		
= fmap id x

		applying first functor law (fmap id = id)
		
= x

# pure (g x) = pure g <*> pure x
pure (g x)

		applying pure
		
= Just (g x)

		unapplying fmap (fmap g (Just x)) = Just (g x))
		
= fmap g (Just x))

		unapplying <*>
		
= (Just g) <*> (Just x) 

		unapplying pure for the first Just
		
= pure g <*> (Just x)

		unapplying pure for Just
		
= pure g <*> pure x

# x <*> pure y = pure (\g -> g y) <*> x
## Case 1 (x = Nothing)
Nothing <*> pure y 

		applying pure
		
= Nothing <*> Just y

		applying <*>
		
= Nothing

		unapplying fmap (fmap f Nothing = Nothing)
		
= fmap f Nothing

		unapplying <*>
		
= Just f <*> Nothing

		unapplying pure
		
= pure f <*> Nothing

		considering f as (\g -> g y)
		
= pure (\g -> g y) <*> Nothing

## Case 2 (x = Just f)
pure (\g -> g y) <*> (Just f) = (Just f) <*> pure y

		applying pure
		
= (Just (\g -> g y)) <*> (Just f)

		applying <*>
		
= fmap (\g -> g y) (Just f)

		applying fmap
		
= Just ((\g -> g y) f)

		applying \g
		
= Just (f y)

		unapplying pure
		
= pure (f y)

		applying II applicative law
		
= pure f <*> pure y

		applying pure
		
= (Just f) <*> pure y

###### or

(Just f) <*> pure y = pure (\g -> g y) <*> (Just f)
(Just f) <*> pure y

		applying pure
		
= (Just f) <*> (Just y)

		applying <*>
		
= fmap f (Just y)

		applying fmap
		
= Just (f y)

		considering f y as ((\g -> g y) f)
		
= Just ((\g -> g y) f)

		applying pure
		
= pure ((\g -> g y) f) 

		applying II applicative law
		
= pure (\g -> g y) <*> pure f

		applying pure
		
= pure (\g -> g y) <*> Just f

# x <*> (y <*> z) = pure (.) <*> x <*> y <*> z
## Case 1 (x = Nothing)
Nothing <*> (y <*> z) 

		unapplying fmap
		
= fmap (.) (Nothing <*> y <*> z)

		unapplying <*>
		
= (Just (.)) <*> (Nothing <*> y <*> z)

		unapplying pure
		
= pure (.) <*> Nothing <*> y <*> z

## Case 2 (x = Just f and y = Nothing)
(Just f) <*> Nothing <*> z

		applying outermost <*>
		
= fmap f (Nothing <*> z)

		applying <*>
		
= fmap f Nothing

