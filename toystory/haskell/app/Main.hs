module Main (
    main
) where

import Data.Map (Map, alter, empty)

data Color = Black | Blue | Red | Green deriving (Eq, Ord, Show)
data Toy = Toy { name :: [Char], color :: Color, broken :: Bool, price :: Double } deriving (Show)

data ToysStats = ToysStats
  { totalCount :: Int,
    totalBlue :: Int,
    totalExpensiveAndBroken :: Int,
    colorDistribution :: Map Color Int
  } deriving (Show)

type Counter = Toy -> ToysStats -> ToysStats

totalCounter :: Counter
totalCounter _ stats = stats { totalCount = totalCount stats + 1 }

blueCounter :: Counter
blueCounter toy stats =
  if color toy == Blue
    then stats { totalBlue = totalBlue stats + 1 }
    else stats

expensiveAndBrokenCounter :: Counter
expensiveAndBrokenCounter toy stats =
  if broken toy && price toy > 10
    then stats { totalExpensiveAndBroken = totalExpensiveAndBroken stats + 1 }
    else stats

colorDistributionCounter :: Counter
colorDistributionCounter toy stats =
  let dist = colorDistribution stats
      update = alter (Just . maybe 1 (+1)) (color toy)
   in stats { colorDistribution = update dist }

calculateStats :: [Toy] -> ToysStats
calculateStats toys =
  let initialStats = ToysStats 0 0 0 empty
      counters = [totalCounter, blueCounter, expensiveAndBrokenCounter, colorDistributionCounter]
      applyCounters toy stats = foldr (\f acc -> f toy acc) stats counters
   in foldr applyCounters initialStats toys


main :: IO ()
main = do
  let toys = [
        Toy "Bear" Black False 11.99,
        Toy "Doll" Red False 4.00,
        Toy "Car" Blue True 9.99,
        Toy "Truck" Blue True 13.00,
        Toy "Spaceship" Green False 13.00
        ]
  print $ calculateStats toys
