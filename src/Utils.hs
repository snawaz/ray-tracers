
module Utils where

import           Data.Time.Clock.System
import           Data.Int

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

getSecondsNow :: IO Int64
getSecondsNow = do
    t <- getSystemTime
    return $ systemSeconds t

getSecondsElapsed :: IO a -> IO (a, Int64)
getSecondsElapsed action = do
    start <- getSecondsNow
    result <- action
    end <- getSecondsNow
    return (result, end - start)

