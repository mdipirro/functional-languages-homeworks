fibs :: [Integer]
fibs =  base ++ (next base)
        where base = [0, 1]

next :: [Integer] -> [Integer]
next fs = f : next (fs ++ [f])
          where sum (a, b)  = a + b
                f           = sum $ last $ zip fs $ tail fs
