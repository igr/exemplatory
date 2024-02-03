
module StopWatch (
    Stopwatch,
    startStopwatch,
    stopStopwatch,
    readStopwatch,
    MonadTime(currentTime),
) where

import Data.Time.Clock (UTCTime, diffUTCTime)

class Monad m => MonadTime m where
    currentTime :: m UTCTime

data Stopwatch = Running UTCTime | Stopped UTCTime UTCTime
    deriving (Eq, Show)

-- Starts the stopwatch. If the stopwatch is already running, it resets the start time.
startStopwatch :: MonadTime m => m Stopwatch
startStopwatch = do
    Running <$> currentTime

-- -- Stops the stopwatch and records the stop time. If the stopwatch is already stopped, this does nothing.
stopStopwatch :: MonadTime m => Stopwatch -> m Stopwatch
stopStopwatch (Running startTime) = do
    Stopped startTime <$> currentTime
stopStopwatch stopwatch@(Stopped _ _) = return stopwatch

readStopwatch :: MonadTime m => Stopwatch -> m Double
readStopwatch (Running startTime) = do
    currentTime >>= \endTime -> return $ realToFrac $ diffUTCTime endTime startTime
readStopwatch (Stopped startTime endTime) =
    return $ realToFrac $ diffUTCTime endTime startTime
