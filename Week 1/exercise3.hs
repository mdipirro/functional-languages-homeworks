toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0    = [] -- empty list if x < 0
  | x < 10    = [x] -- stop the recursion if there is only one number
  -- otherwise take the last digit and call itself with the same number
  -- without the last digit
  | otherwise = toDigits (x `div` 10) ++ [x - 10 * (x `div` 10)]

{-|
  The 'sumDigits' function calculate the sum of all digits of the numbers in
  the list.
  It takes one argument, of tpe '[Integer]', which represents the list.
-}
sumDigits :: [Integer] -> Integer
sumDigits []        = 0
sumDigits (a:[])    = sum (toDigits a)
sumDigits (a:list)  = sumDigits (toDigits a) + sumDigits list
