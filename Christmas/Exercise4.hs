fibs :: [Integer]
fibs =  base ++ (next base)
        where base = [0, 1]

{-|
`f`is the (n+1)-th Fibonacci's number. Starting from a list containing n Fibonacci's
numbers, `next` computes the next number in a lazy way.
`uncurry (+)` gives as a result something like [(\(a, b) -> a + b)]
`last` takes the last pair of the list
`zip fs $ tail fs` gives a list of pair like [(n, n-1)], where `n` and `n-1` are
the n-th and the (n-1)-th Fibonacci's numbers.
The (n+1)-th number is computed iff the computation is necessary. Otherwise the
computation stops.
-}
next :: [Integer] -> [Integer]
next fs = f : next (fs ++ [f])
          where f = uncurry (+) $ last $ zip fs $ tail fs
