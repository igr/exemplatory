module Main (
    main
) where

import Data.Map (Map, alter, empty)

data Color = Black | Blue | Red | Green deriving (Eq, Ord, Show)
data Toy = Toy { name :: [Char], color :: Color, broken :: Bool, price :: Double } deriving (Show)

newtype TotalCount = TotalCount Int deriving (Show)
newtype TotalBlue = TotalBlue Int deriving (Show)
newtype TotalExp = TotalExp Int deriving (Show)
newtype ColorDist = ColorDist (Map Color Int) deriving (Show)

data ToysStats = ToysStats
  { totalCount :: TotalCount,
    totalBlue :: TotalBlue,
    totalExpensiveAndBroken :: TotalExp,
    colorDistribution :: ColorDist
  } deriving (Show)

type Counter = Toy -> ToysStats -> ToysStats

totalCounter :: Counter
totalCounter _ stats = let TotalCount n = totalCount stats in
                       stats { totalCount = TotalCount (n + 1) }

blueCounter :: Counter
blueCounter toy stats =
  if color toy == Blue
    then let TotalBlue n = totalBlue stats in
         stats { totalBlue = TotalBlue (n + 1) }
    else stats

expensiveAndBrokenCounter :: Counter
expensiveAndBrokenCounter toy stats =
  if broken toy && price toy > 10
    then let TotalExp n = totalExpensiveAndBroken stats in
         stats { totalExpensiveAndBroken = TotalExp (n + 1) }
    else stats

colorDistributionCounter :: Counter
colorDistributionCounter toy stats =
  let ColorDist dist = colorDistribution stats
      update = alter (Just . maybe 1 (+1)) (color toy)
   in stats { colorDistribution = ColorDist $ update dist }

calculateStats :: [Toy] -> ToysStats
calculateStats toys =
  let initialStats = ToysStats (TotalCount 0) (TotalBlue 0) (TotalExp 0) (ColorDist empty)
      counters = [
        totalCounter,
        blueCounter,
        expensiveAndBrokenCounter,
        colorDistributionCounter
        ]
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
