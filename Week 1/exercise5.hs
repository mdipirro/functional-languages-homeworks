type Peg = String
type Move = (Peg, Peg)

--------------------------------------------------------------------------------
---------------------------------Exercise 3-------------------------------------
--------------------------------------------------------------------------------

{-|
The Towers of Hanoi is a classic puzzle with a solution that can be described
recursively. Disks of different sizes are stacked on three pegs; the goal is to
get from a starting configuration with all disks stacked on the first peg to an
ending configuration with all disks stacked on the last peg.
The only rules are:
  -) you may only move one disk at a time
  -) a larger disk may never be stacked on top of a smaller one
To move n discs (stacked in increasing size) from peg a to peg b using peg c as
temporary storage:
  1) move n − 1 discs from a to c using b as temporary storage
  2) move the top disc from a to b
  3) move n − 1 discs from c to b using a as temporary storage

'hanoi' is a function which resolves the Towers of Hanoi game. Given the number
of discs and names for the three pegs, 'hanoi' returns a list of moves to be
performed to move the stack of discs from the first peg to the second.
-}
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi(n-1) c b a
