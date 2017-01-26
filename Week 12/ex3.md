# fmap id = id
## Base case (Leaf x)
fmap id (Leaf x)

		applying fmap
		
= Leaf (id x)

		applying id
		
= (Leaf x)

		unapplying id
		
= id (Leaf x)

## Inductive case (Node l r)
fmap id (Node l r)

		applying fmap
		
= Node (fmap id l) (fmap id r)

		induction hypothesis on the first fmap
		
= Node l (fmap id r)

		induction hypothesis
		
= Node l r

		unapplying id
		
= id (Node l r)

# fmap (f . g) = fmap f . fmap g
## Base case (Leaf x)
fmap (f . g) (Leaf x) 

		applying fmap
		
= Leaf ((f . g) x)

		applying (.)
		
= Leaf (f (g x))

		unapplying fmap
		
= fmap f (Leaf (g x))

		unapplying fmap
		
= fmap f (fmap g (Leaf x))

		unapplying (.)
		
= (fmap f . fmap g) Leaf x

## Inductive case (Node l r)
fmap (f . g) (Node l r)

		applying fmap
		
= Node (fmap (f . g) l) (fmap (f . g) r)

		induction hypothesis on the first fmap
		
= Node ((fmap f . fmap g) l) (fmap (f . g) r)

		induction hypothesis on the third fmap
		
= Node ((fmap f . fmap g) l) ((fmap f . fmap g) r)

		unapplying fmap
		
= (fmap f . fmap g) (Node l r)