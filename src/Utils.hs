
module Utils(
    rotate,
    getSecondsNow,
    getSecondsElapsed,
    getCurrentTimeString,
    printCurrentTime
) where

import           Data.Time.Clock        (getCurrentTime)
import           Data.Time.Clock.System (getSystemTime, systemNanoseconds, systemSeconds)
import           Data.Time.Format       (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime    (getCurrentTimeZone, utcToZonedTime)
-- import           Data.Int

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

getSecondsNow :: IO Double
getSecondsNow = do
    t <- getSystemTime
    let s1 = fromIntegral (systemSeconds t)
    let s2 = fromIntegral (systemNanoseconds t) / fromIntegral 1000000000
    return $ s1 + s2

getSecondsElapsed :: IO a -> IO (a, Double)
getSecondsElapsed action = do
    start <- getSecondsNow
    result <- action
    end <- getSecondsNow
    return (result, end - start)

getCurrentTimeString :: IO String
getCurrentTimeString = do
    startTime <- getCurrentTime
    tz <- getCurrentTimeZone
    let now = utcToZonedTime tz startTime
    return $ formatTime defaultTimeLocale "%c" now

printCurrentTime :: (String -> String) -> IO ()
printCurrentTime f = do
    now <-getCurrentTimeString
    putStrLn $ f now
