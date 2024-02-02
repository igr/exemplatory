module StopWatch (
    Stopwatch,
    startStopwatch,
    stopStopwatch,
    readStopwatch
) where

import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)

data Stopwatch = Running UTCTime | Stopped UTCTime UTCTime
    deriving (Eq, Show)

-- Starts the stopwatch. If the stopwatch is already running, it resets the start time.
startStopwatch :: IO Stopwatch
startStopwatch = Running <$> getCurrentTime

-- Stops the stopwatch and records the stop time. If the stopwatch is already stopped, this does nothing.
stopStopwatch :: Stopwatch -> IO Stopwatch
stopStopwatch (Running startTime) = Stopped startTime <$> getCurrentTime
stopStopwatch stopwatch@(Stopped _ _) = return stopwatch

-- Reads the elapsed time from the stopwatch in seconds. If the stopwatch is running, it calculates the time until now.
readStopwatch :: Stopwatch -> IO Double
readStopwatch (Running startTime) = do
    currentTime <- getCurrentTime
    return $ realToFrac $ diffUTCTime currentTime startTime
readStopwatch (Stopped startTime stopTime) =
    return $ realToFrac $ diffUTCTime stopTime startTime
