toDigits :: Integer -> [Integer]
toDigits x
    | x < 10    = [x]
    | otherwise = toDigits (x `div` 10) ++ [x - 10 * (x `div` 10)]
