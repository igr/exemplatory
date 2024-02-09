module Main (
    main
) where

import Control.Concurrent (threadDelay)
import StopWatch.Api (StopWatch (startStopWatch, stopStopWatch, readStopWatch))
import StopWatch.IO (makeIOStopWatch)
import Data.Time (getCurrentTime)

main :: IO ()
main = do
    putStrLn "Starting the stopwatch..."
    stopwatch <- makeIOStopWatch getCurrentTime
    startStopWatch stopwatch
    threadDelay 1000000
    putStrLn "Stopping the stopwatch..."
    stopStopWatch stopwatch
    readStopWatch stopwatch >>= print
    putStrLn "Done!"
