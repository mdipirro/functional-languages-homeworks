# exec (c ++ d) s = exec d (exec c s)
## Base case (c = [])
exec ([] ++ d) s 

  applying ++

= exec d s

  unapplying exec

= exec d (exec [] s)

## Inductive case
### Case 1 ((PUSH n):c)
exec (((PUSH n):c) ++ d) s

  applying ++

= exec ((PUSH n) : (c++d)) s

  applying exec

= exec (c++d) (n:s)

  induction hypothesis

= exec d (exec c (n:s))

  unapplying exec

= exec d (exec (PUSH n):c) s)

### Case 2 (ADD:c)
exec ((ADD:c) ++ d) s

  applying ++

= exec (ADD : (c++d)) s

  considering s in the form of x:y:s'

= exec (ADD : (c++d)) (x:y:s')

  applying exec

= exec (c++d) ((x+y) : s')

  induction hypothesis

= exec d (exec c ((x+y) : s'))

  unapplying exec

= exec d (exec (ADD:c) (x:y:s'))

  restoring s in the original form

= exec d (exec (ADD:c) s) 
