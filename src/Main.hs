

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           BaseVec
import           Hittable
import           Image

samplePerPixels = 4

world = HittableList [
        (Sphere (vec 0 0 (-1)) 0.5),
        (Sphere (vec 0 (-100.5) (-1)) 100)
    ]

main :: IO ()
main = do
    -- writeImage 1800
    -- writeImage 800
    -- writeImage 84 samplePerPixels world
    writeImage 384 100 world
    --writeImage 4 samplePerPixels world
