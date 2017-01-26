# comp' e c = comp e ++ c
## Base case (e = Val n)
		comp' (Val n) c 

applying equation
		
		= comp (Val n) ++ c

applying comp
		
		= [PUSH n] ++ c

simplifying ++
		
		= (PUSH n) : c

## Inductive case (e = ADD x y)
		comp' (ADD x y) c

applying equation
		
		= comp (ADD x y) ++ c

applying comp
		
		= comp x ++ (comp y ++ [ADD] ++ c)

inductive hypothesis on comp x
		
		= comp' x (comp y ++ [ADD] ++ c)

inductive hypothesis on comp y
		
		= comp' x (comp' y ([ADD] ++ c))

simplifying ++
		
		= comp' x (comp' y (ADD : c))

Hence, we can define comp' as follows

		comp' (VAL n) c = (PUSH n) c
		comp' (ADD x y) c = comp' x (comp' y (ADD : c))