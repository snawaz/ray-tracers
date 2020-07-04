

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           System.Random

import           BaseVec
import           Hittable
import           Image
import           Samplings


world = HittableList [
        (Sphere (vec 0 0 (-1)) 0.5),
        (Sphere (vec 0 (-100.5) (-1)) 100)
    ]

main :: IO ()
main = do
    saveImage 
    -- testCode

testCode = do
    let g = mkStdGen 22
    print "test Code"
    print $ samplePointInCircle g 1

saveImage = do
    -- writeImage 1800
    -- writeImage 800
    -- writeImage 384 100 world
    writeImage 254 20 world
    -- writeImage 100 10 world
    -- writeImage 20 5 world
    -- writeImage 4 samplePerPixels world
