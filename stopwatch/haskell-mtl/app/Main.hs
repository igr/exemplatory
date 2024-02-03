{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (
    main
) where

import StopWatch
import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Cont

newtype MyIO a = MyIO { run :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTime MyIO where
    currentTime = MyIO Data.Time.Clock.getCurrentTime

-- IO instance for MonadTime
-- This is an alternative, nut creates an orphan instance
-- instance MonadTime IO where
--     currentTime :: IO UTCTime
--     currentTime = Data.Time.Clock.getCurrentTime

main :: IO ()
main = do
    putStrLn "Starting the stopwatch..."
    sw <- run startStopwatch
    threadDelay 1000000
    stopped <- run $ stopStopwatch sw
    elapsed <- run $ readStopwatch stopped
    putStrLn $ "Stopwatch stopped. Total elapsed time: " ++ show elapsed ++ " seconds."
