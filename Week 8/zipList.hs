{-|
--------------------------------------------------------------------------------
-----------------------------------Exercise 1-----------------------------------
--------------------------------------------------------------------------------
-}
newtype ZipList a = Z [a]
                deriving (Show)

instance Functor ZipList where
fmap f (Z xs) = Z (map f xs)

instance Applicative ZipList where
pure x = Z (repeat x)
Z fs <*> Z xs = Z (zipWith (\f x -> f x) fs xs)

{-|
--------------------------------------------------------------------------------
-----------------------------------Exercise 2-----------------------------------
--------------------------------------------------------------------------------
-}
{-|
Write an Haskell program that, given three lists L1, l2 and L3 of length n >0,
computes the list containing all lists of three elements where the first element
comes from L1, the second element from L2 and the third from L3.
-}
-- This function uses the normal lists and not the ZipLists. This because the
-- exercise requires *all lists*, so a cartesian product is required.
allLists :: [a] -> [a] -> [a] -> [[a]]
allLists xs ys zs = (\x y z -> [x, y, z]) <$> xs Prelude.<*> ys Prelude.<*> zs

{-|
Write an Haskell program that, given lists L1, L2 and L3 of length n>0, computes
the list containing n lists of three elements where for the i-th such list, the
first element is the i-th element of L1, the second element is the i-th element
of L2 and the third is the i-th element of L3.
-}
-- Here I use ZipLists so I can apply operation between elements which are in
-- the same position in the input lists.
takeElements :: [a] -> [a] -> [a] -> [[a]]
takeElements xs ys zs = (\(Z e) -> e) (Main.pure (\x y z -> [x, y, z]) Main.<*> (Z xs) Main.<*> (Z ys) Main.<*> (Z zs))

{-|
Write an Haskell program that, given a list L of lists of Int, produces a new
list that is obtained from L by adding 1 to the first list contained in L, then
adding 2 to the second list of L ad so on.
-}
-- Here I use again ZipLists, but this time is a bit more complicated. Firts of
-- all I create the 'increments' list, and I do that using recursion on 'n', a
-- parameter which is incremented through the input list. Then I use the
-- applicative style for performing the required operation. For each list 'is'
-- in the input list I sum every element of 'is' with the right incrment. After
-- that I remove the ZipList constructor using a lambda.
incrementalAddition :: [[Int]] -> [[Int]]
incrementalAddition iss = map (\(Z e) -> e) list
                          where (Z list) = Main.pure sum Main.<*> (Z (increments 1 iss)) Main.<*> (Z iss)
                                sum xs is = Main.pure (+) Main.<*> (Z xs) Main.<*> (Z is)
                                increments _ [] = []
                                increments n (is:iss) = (replicate (length is) n) : (increments (n + 1) iss)
