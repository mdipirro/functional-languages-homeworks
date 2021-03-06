--------------------------------------------------------------------------------
---------------------------------Exercise 1-------------------------------------
--------------------------------------------------------------------------------
{-|
  The 'toDigits' function converts positive integers to a list of digits.
  It takes one argument, of type 'Integer', which represents the number.
-}
toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

{-|
  The 'toDigitsRev' function converts positive integers to a list of digits,
  but the order of the digits is opposite compared to the number digits.
  It takes one argument, of type 'Integer', which represents the number.
-}
toDigitsRev :: Integer -> [Integer]
toDigitsRev x = if x <= 0
                then []
                else (x `mod` 10) : (toDigitsRev (x `div` 10))

--------------------------------------------------------------------------------
---------------------------------Exercise 2-------------------------------------
--------------------------------------------------------------------------------
{-|
  The 'doubleEveryOther' function double every other number beginning from the
  right; that is, the second-to-last, fourth-to-last, ..., numbers are doubled.
  It takes one argument, of tpe '[Integer]', which represents the list.
-}
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEvenIndexed (reverse xs))

{-|
  The 'doubleEvenIndexed' function double every even-indexed number beginning
  from the left; that is, the second, fourth, ..., numbers are doubled.
  It takes one argument, of tpe '[Integer]', which represents the list.
-}
doubleEvenIndexed :: [Integer] -> [Integer]
doubleEvenIndexed []          = []
doubleEvenIndexed (x1:x2:xs)  = [x1] ++ [2 * x2] ++ doubleEvenIndexed xs
doubleEvenIndexed xs          = xs

--------------------------------------------------------------------------------
---------------------------------Exercise 3-------------------------------------
--------------------------------------------------------------------------------
{-|
  The 'sumDigits' function calculate the sum of all digits of the numbers in
  the list.
  It takes one argument, of tpe '[Integer]', which represents the list.
-}
sumDigits :: [Integer] -> Integer
sumDigits []      = 0
sumDigits (x:xs)  = sum (toDigits x) + sumDigits xs

--------------------------------------------------------------------------------
---------------------------------Exercise 4-------------------------------------
--------------------------------------------------------------------------------
{-|
  Finally validate a credit card number! The 'validate' function validates an
  US credit card number.
  It takes one argument, a number, and return the value True iff
  the incoming number is a valid credit card number, False otherwise.
-}
validate :: Integer -> Bool
validate x =  sumDigits(doubleEveryOther (toDigits x)) `mod` 10 == 0
