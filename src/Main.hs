

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           System.Random
import           System.Environment

import           BaseVec
import           Hittable
import           Image
import           Samplings
import           Scenes
import           Utils

r = cos (pi/4)
world = HittableList [
        -- (Sphere (vec (-r) 0 (-1)) r (Material (Lambertian (vec 0 0 1)))),
        -- (Sphere (vec r 0 (-1)) r (Material (Lambertian (vec 1 0 0))))
        (Sphere (vec 0 0 (-1)) 0.5 (Material (Lambertian (vec 0.1 0.2 0.5)))),
        (Sphere (vec 0 (-100.5) (-1)) 100 (Material (Lambertian (vec 0.8 0.8 0.0)))),
        (Sphere (vec 1 0 (-1)) 0.5 (Material (Metal (vec 0.8 0.6 0.2) 0.3))),
        (Sphere (vec (-1) 0 (-1)) 0.5 (Material (Dielectric 1.5))),
        (Sphere (vec (-1) 0 (-1)) (-0.45) (Material (Dielectric 1.5)))
    ]

main :: IO ()
main = do
    args <- getArgs
    let [width, samplePerPixels, depth] = fmap read args
    saveImage width samplePerPixels depth

saveImage :: Int -> Int -> Int -> IO ()
saveImage width samplePerPixels depth = do
    -- writeImage 1800
    -- writeImage 384 100 world
    (elapsed, _) <- getSecondsElapsed $ writeImage width samplePerPixels (randomScene 23) depth
    putStrLn $ "\n TOTAL TIME ELAPSED: " ++ show (elapsed)
    -- writeImage 200 100 (randomScene 23) 50
    -- writeImage 384 100 (randomScene 23) 50
    -- writeImage 800 100 (randomScene 23) 50
    -- writeImage 1200 100 (randomScene 23) 50
