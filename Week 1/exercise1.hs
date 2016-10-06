{-|
  The 'toDigits' function converts positive integers to a list of digits.
  It takes one argument, of type 'Integer', which represents the number.
-}
toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0    = [] -- empty list if x < 0
  | x < 10    = [x] -- stop the recursion if there is only one number
  -- otherwise take the last digit and call itself with the same number
  -- without the last digit
  | otherwise = toDigits (x `div` 10) ++ [x - 10 * (x `div` 10)]

  {-|
    The 'toDigitsRev' function converts positive integers to a list of digits,
    but the order of the digits is opposite compared to the number digits.
    It takes one argument, of type 'Integer', which represents the number.
  -}
toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)
