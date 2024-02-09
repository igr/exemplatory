{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module StopWatch.Api (
    StopWatch(..)
)
where

data StopWatch m = StopWatch {
        startStopWatch :: m (),
        stopStopWatch :: m (),
        readStopWatch :: m Double
    }
