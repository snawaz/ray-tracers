

{-# LANGUAGE PatternSynonyms #-}

module Main(main) where

import           System.Environment (getArgs)

import           Image              (writeImage)
import           Scenes             (randomScene)
import           Utils              (getSecondsNow, printCurrentTime)

main :: IO ()
main = do
    printCurrentTime $ \now ->  "Program started at " ++ now
    args <- getArgs
    let [width, samplesPerPixel, raysPerSample] = fmap read args
    seed <- getSecondsNow
    _filename <- writeImage width samplesPerPixel raysPerSample (randomScene (floor seed))
    printCurrentTime $ \now -> "Program stopping at " ++ now
