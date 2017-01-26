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
## Case 1 (at least one among *x*, *y* or *z* is Nothing)
### Lemma: a <*> Nothing = Nothing
#### a = Nothing
Nothing <*> Nothing 
		applying <*>

= Nothing

#### a = Just a'
(Just a') <*> Nothing

		applying <*>
		
= fmap a' Nothing

		applying fmap
		
= Nothing

Hence, using the last lemma, if any of *x*, *y* or *z* is Nothing, the entire expression becomes Nothing. Since Nothing = Nothing the law holds.

## Case 2 (x = Just u, y = Just v and z = Just w -> OK because if any of them is Nothing we are in **Case 1**)
(Just u) <*> ((Just v) <*> (Just w))

		applying outermost <*>
		
= fmap u ((Just v) <*> (Just w))

		applying <*>
		
= fmap u (fmap v (Just w))

		applying innermost fmap
		
= fmap u (Just (v w))

		applying fmap
		
= Just (u (v w))

		unapplying pure
		
= pure (u (v w))

		unapplying (.)
		
= pure ((u . v) w)

		applying II applicative law
		
= pure ((u .) v) <*> pure w

		applying II applicative law
		
= pure (u .) <*> pure v <*> pure w

		rewriting (.) as prefix
		
= pure ((.) u) <*> pure v <*> pure w

		applying II functor law
		
= pure (.) <*> pure u <*> pure v <*> pure w

		applying pure
		
= pure (.) <*> Just u <*> pure v <*> pure w

		applying pure

= pure (.) <*> Just u <*> Just v <*> pure w

		applying pure

= pure (.) <*> Just u <*> Just v <*> Just w

		

		

