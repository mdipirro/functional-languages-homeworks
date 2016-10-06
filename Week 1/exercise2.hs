{-|
  The 'doubleEveryOther' function double every other number beginning from the
  right; that is, the second-to-last, fourth-to-last, ..., numbers are doubled.
  It takes one argument, of tpe '[Integer]', which represents the list.
-}
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = reverse (doubleEvenIndexed (reverse list))

{-|
  The 'doubleEvenIndexed' function double every even-indexed number beginning
  from the left; that is, the second, fourth, ..., numbers are doubled.
  It takes one argument, of tpe '[Integer]', which represents the list.
-}
doubleEvenIndexed :: [Integer] -> [Integer]
doubleEvenIndexed []          = []
doubleEvenIndexed (a:b:list)  = [a] ++ [2 * b] ++ doubleEvenIndexed list
doubleEvenIndexed list        = list
