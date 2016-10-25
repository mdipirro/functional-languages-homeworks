type Peg = String
type Move = (Peg, Peg)

--------------------------------------------------------------------------------
---------------------------------Exercise 5-------------------------------------
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
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

--------------------------------------------------------------------------------
---------------------------------Exercise 6-------------------------------------
--------------------------------------------------------------------------------
{-|
What if there are four pegs instead of three?
That is, the goal is still to move a stack of discs from the first peg to
the last peg, without ever placing a larger disc on top of a smaller
one, but now there are two extra pegs that can be used as “temporary”
storage instead of only one. Write a function similar to hanoi
which solves this problem in as few moves as possible.
In this solution I used the Frame–Stewart algorithm, which gives an optimal
solution for four (and conjecturally for even more) pegs. The steps are the
following:
1) for some k, 1 <= k < n, move the top k disks to a single peg other than the
start or destination pegs
2) without disturbing the peg that now contains the top k disks, transfer the
remaining n-k disks to the destination peg, using only the remaining 3 pegs
3) finally, transfer the top k disks to the destination peg.
The optimal k for four pegs is defined as `n - round[sqrt(2*n + 1)] + 1`.
-}
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ =  []
hanoi4 1 a b _ _ =  [(a, b)]
hanoi4 n a b c d =  hanoi4 k a c b d ++ hanoi (n - k) a b d  ++ hanoi4 k c b a d
                    where k = n - round (sqrt (fromIntegral (2 * n + 1))) + 1
