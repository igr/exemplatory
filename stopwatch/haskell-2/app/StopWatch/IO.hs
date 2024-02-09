module StopWatch.IO (
    StopwatchState,
    makeIOStopWatch
) where

import StopWatch.Api
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.IORef

data StopwatchState = Running UTCTime | Stopped UTCTime UTCTime
    deriving (Eq, Show)

makeIOStopWatch :: IO UTCTime -> IO (StopWatch IO)
makeIOStopWatch askTime = do
    currentTimeStart <- askTime
    stateRef <- newIORef (Running currentTimeStart)
    pure $ StopWatch {

        startStopWatch = do
           currentTime <- askTime
           writeIORef stateRef (Running currentTime),

        stopStopWatch = do
           currentTime <- askTime
           currentState <- readIORef stateRef
           case currentState of
                Running startTime -> writeIORef stateRef (Stopped startTime currentTime)
                Stopped _ _ -> pure (),
        readStopWatch = do
              currentTime <- askTime
              currentState <- readIORef stateRef
              case currentState of
                 Running startTime -> pure $ realToFrac $ diffUTCTime currentTime startTime
                 Stopped startTime stopTime -> pure $ realToFrac $ diffUTCTime stopTime startTime
    }
