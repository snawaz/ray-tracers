
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE PatternSynonyms #-}

module Main(main) where

import           System.Environment (getArgs)

import           Image              (writeImage)
import           Scenes             (randomScene)
import           Utils              (printCurrentTime)

main :: IO ()
main = do
    printCurrentTime $ \now ->  "Program started at " ++ now
    args <- getArgs
    let [width, samplesPerPixel, raysPerSample] = fmap read args
    let scene = randomScene 23
    writeImage width samplesPerPixel raysPerSample scene
    printCurrentTime $ \now -> "Program stopping at " ++ now
