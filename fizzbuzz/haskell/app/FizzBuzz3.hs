{-# LANGUAGE StandaloneDeriving #-}

module FizzBuzz3 (
    fizzbuzz3
) where

import Data.Array

data DIGIT = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Eq, Enum, Bounded)
deriving instance Ix DIGIT
deriving instance Ord DIGIT

instance Show DIGIT where
    show d = show (fromEnum d)

data NUMBER = NUMBER {f:: DIGIT, s :: DIGIT, t :: DIGIT, fo :: DIGIT}

instance Show NUMBER where
    show (NUMBER f s t fo) = show f ++ show s ++ show t ++ show fo

parseString :: String -> NUMBER
parseString n =
    let d1 = charToDigit (n !! 0)
        d2 = charToDigit (n !! 1)
        d3 = charToDigit (n !! 2)
        d4 = charToDigit (n !! 3)
    in NUMBER d1 d2 d3 d4

charToDigit :: Char -> DIGIT
charToDigit c
    | c >= '0' && c <= '9' = digits !! (fromEnum c - fromEnum '0')
    | otherwise = error "Invalid character for DIGIT"
    where
    digits = [D0, D1, D2, D3, D4, D5, D6, D7, D8, D9]

fizzbuzz3 :: String -> String
fizzbuzz3 n =
    let num = parseString n
    in _fizzbuzzNumber num

_fizzbuzzNumber :: NUMBER -> String
_fizzbuzzNumber num
    | isDivisibleBy "3" num && isDivisibleBy "5" num = "FizzBuzz"
    | isDivisibleBy "3" num = "Fizz"
    | isDivisibleBy "5" num = "Buzz"
    | otherwise = show num

digitRemaindersArray :: Array DIGIT Int
digitRemaindersArray = listArray (D0, D9) [0, 1, 2, 0, 1, 2, 0, 1, 2, 0]

digitRemainders :: DIGIT -> Int
digitRemainders d = digitRemaindersArray ! d

isDivisibleBy :: String -> NUMBER -> Bool
isDivisibleBy "3" (NUMBER f s t fo) =
    let totalRemainder = (digitRemainders f + digitRemainders s + digitRemainders t + digitRemainders fo) `mod` 3
    in totalRemainder == 0

isDivisibleBy "5" (NUMBER _ _ _ fo) =
    fo `elem` [D0, D5]
