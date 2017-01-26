# xs ++ [] = xs
## Base case (xs = [])
		[] ++ [] 
  
applying ++
	
		= [] = xs

## Inductive case (x:xs)
		(x:xs) ++ []
  
applying ++
	
		= x : (xs ++ [])

induction hypothesis
	
		= x : xs

# xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
## Base case (xs = [])
		[] ++ (ys ++ zs)
  
applying ++
	
		= ys ++ zs

unapplying ++
	
		= ([] ++ ys) ++ zs

## Inductive case (x:xs)
		(x:xs) ++ (ys ++ zs)
  
applying ++
    
		= x : (xs ++ (ys ++ zs))

induction hypothesis
	
		= x : ((xs ++ ys) ++ zs)

unapplying ++
	
		= (x : (xs ++ ys)) ++ zs

unapplying ++
	
		= ((x:xs) ++ ys) ++ zs