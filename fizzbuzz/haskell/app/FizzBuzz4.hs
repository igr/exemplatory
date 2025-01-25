module FizzBuzz4 (
    fizzbuzz4
) where

-- Only for printing purposes, does not count in the solution
fizzbuzz4 :: String -> String
fizzbuzz4 n = case fizzbuzz4' n of
    Just s -> s
    Nothing -> "[" ++ n ++ "]"

-- FizzBuzz starts here
fizzbuzz4' :: String -> Maybe String
fizzbuzz4' "0" = Just "0" -- handle zero
fizzbuzz4' n
    | head n == '-' = Nothing  -- handle negative numbers
    | '.' `elem` n = Nothing   -- handle decimal numbers
    | isDivisibleBy "3" n && isDivisibleBy "5" n = Just "FizzBuzz"
    | isDivisibleBy "3" n = Just "Fizz"
    | isDivisibleBy "5" n = Just "Buzz"
    | otherwise = Just n  -- return itself

isDivisibleBy :: String -> String -> Bool

-- Check divisibility by 3
isDivisibleBy "3" n = case sumRemainders of
    Just total -> total `mod` 3 == 0
    Nothing -> False  -- if digit is invalid
  where sumRemainders = mapM digitRemainder n >>= return . sum

-- Check divisibility by 5
isDivisibleBy "5" n = last n `elem` "05"

-- A lookup table for remainders of digits '0' to '9' modulo 3
digitRemaindersTable :: [(Char, Int)]
digitRemaindersTable = [('0', 0), ('1', 1), ('2', 2), ('3', 0), ('4', 1),
                        ('5', 2), ('6', 0), ('7', 1), ('8', 2), ('9', 0)]
-- Remainder of a digit using the above lookup table
digitRemainder :: Char -> Maybe Int
digitRemainder d = lookup d digitRemaindersTable
