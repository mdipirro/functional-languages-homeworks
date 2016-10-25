-- foldr definition
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v []     = v
foldr f v (x:xs) = f x (Main.foldr f v xs)

type Bit = Int

{-|
bit2int converts a list of Bit, which represents a number encoded in 
base 2, into the decimal representation.
Given a binary number in the form b = [x1,x2,x3,..., xn], the decimal 
representation D can be obtained in the following way:
D = x1 * 2 ^ n + ... + xn * 2 ^ 0
If we reverse the list, we obtain:
D = xn * 2 ^ 0 + ... + x1 * 2 ^ n = xn + 2 * (... + xn * 2 ^ (n-1)) =
  = xn + 2 * (2 * ... + xn * 2 ^ (n-2)) and so on.
I define bit2int in this way, so I can use foldr. Starting from 0, the 
zero element which corresponds to the empty list, for each element x I 
sum x to 2 * v. The only non-multiplied element will be the firt in the
reversed list (the last in the original list).
-}
bit2int :: [Bit] -> Int
bit2int xs = Main.foldr (\x v -> 2 * v + x) 0 (reverse xs)
