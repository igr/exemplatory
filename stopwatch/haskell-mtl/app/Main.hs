module Main (
    main
) where

import StopWatch
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
    putStrLn "Starting the stopwatch..."
    sw <- startStopwatch
    threadDelay 1000000
    stopped <- stopStopwatch sw
    elapsed <- readStopwatch stopped
    putStrLn $ "Stopwatch stopped. Total elapsed time: " ++ show elapsed ++ " seconds."
