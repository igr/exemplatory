module Main (
    main
) where

import Data.Array
import FizzBuzz3
import FizzBuzz4

main :: IO ()
main = do
    putStrLn "Starting the stopwatch..."
    let n = 100
    mapM_ putStrLn $ map fizzbuzz [1..n]
    putStrLn "--------------"
    putStrLn $ fizzbuzz3 "0001"
    putStrLn $ fizzbuzz3 "0003"
    putStrLn $ fizzbuzz3 "0004"
    putStrLn $ fizzbuzz3 "0005"
    putStrLn $ fizzbuzz3 "0014"
    putStrLn $ fizzbuzz3 "0015"
    --putStrLn $ fizzbuzz3 "xxx"
    putStrLn "--------------"

    putStrLn $ fizzbuzz4 "0"
    putStrLn $ fizzbuzz4 "1"
    putStrLn $ fizzbuzz4 "3"
    putStrLn $ fizzbuzz4 "4"
    putStrLn $ fizzbuzz4 "5"
    putStrLn $ fizzbuzz4 "14"
    putStrLn $ fizzbuzz4 "15"
    putStrLn $ fizzbuzz4 "xxx"
    putStrLn $ fizzbuzz4 "-2"
    putStrLn $ fizzbuzz4 "1.4"


fizzbuzz :: Int -> String
fizzbuzz n =
    if n `mod` 15 == 0 then "FizzBuzz"
    else if n `mod` 3 == 0 then "Fizz"
    else if n `mod` 5 == 0 then "Buzz"
    else show n
