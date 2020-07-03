

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           BaseVec
import           Hittable
import           Image

samplePerPixels = 100

world = HittableList [
        (Sphere (vec 0 0 (-1)) 0.5),
        (Sphere (vec 0 (-100.5) (-1)) 100)
    ]

main :: IO ()
main = do
    -- writeImage 1800
    -- writeImage 800
    writeImage 384 samplePerPixels world
    -- testTools

kolor :: Kolor -> Kolor
kolor a = a

testTools = do
    let n = 2 :: Int
    let a = (from 5)  :: Kolor
    let b = (vec 1 2 3) :: Kolor
    print $ a + b
    print $ a - b
    print $ a * b
    print $ kolor $ a - b .* 2
    print $ kolor $ a - (b .* 2)
    print $ kolor $ (a - b) .* 2
    -- print $ 3 .+ a
    -- print $ a .+ n
    -- print $ a .- n
    -- print $ a .* n
    -- print $ a ./ n
    -- let n = 2.0 :: Double
    -- let a = (from 5)  :: Vector
    -- let b = (vec 1 2 3) :: Vector
    -- print $ a + b
    -- print $ a - b
    -- print $ a * b
    -- print $ a .+ n
    -- print $ a .- n
    -- print $ a .* n
    -- print $ a ./ n
    print "DONE"
