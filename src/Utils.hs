
module Utils where

import           Data.Time.Clock.System
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Format
import           Data.Int

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

getSecondsNow :: IO Int64
getSecondsNow = do
    t <- getSystemTime
    return $ systemSeconds t

getMicrosecondsNow :: IO Int64
getMicrosecondsNow = do
    t <- getSystemTime
    let ms1 = (systemSeconds t) * 1000000
    let ms2 = fromIntegral $ (systemNanoseconds t) `div` fromIntegral 1000
    return $ ms1 + ms2
    
getSecondsElapsed :: IO a -> IO (a, Int64)
getSecondsElapsed action = do
    start <- getSecondsNow
    result <- action
    end <- getSecondsNow
    return (result, end - start)

getMicrosecondsElapsed :: IO a -> IO (a, Int64)
getMicrosecondsElapsed action = do
    start <- getMicrosecondsNow
    result <- action
    end <- getMicrosecondsNow
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
