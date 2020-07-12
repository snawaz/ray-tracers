
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main(main) where

import           System.Environment (getArgs)

import           Image              (writeImage)
import           Scenes             (randomScene)
import           Utils              (getSecondsNow, printCurrentTime)

main :: IO ()
main = do
    printCurrentTime $ \now ->  "Program started at " ++ now
    args <- getArgs
    seed <- getSecondsNow
    let [width, samplesPerPixel, raysPerSample] = fmap read args
    let scene = randomScene (floor seed)
    writeImage width samplesPerPixel raysPerSample scene
    printCurrentTime $ \now -> "Program stopping at " ++ now
