

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


r = cos (pi/4)
world = HittableList [
        (Sphere (vec (-r) 0 (-1)) r (Material (Lambertian (vec 0 0 1)))),
        (Sphere (vec r 0 (-1)) r (Material (Lambertian (vec 1 0 0))))
        -- (Sphere (vec 0 0 (-1)) 0.5 (Material (Lambertian (vec 0.1 0.2 0.5)))),
        -- (Sphere (vec 0 (-100.5) (-1)) 100 (Material (Lambertian (vec 0.8 0.8 0.0)))),
        -- (Sphere (vec 1 0 (-1)) 0.5 (Material (Metal (vec 0.8 0.6 0.2) 0.3))),
        -- (Sphere (vec (-1) 0 (-1)) 0.5 (Material (Dielectric 1.5))),
        -- (Sphere (vec (-1) 0 (-1)) (-0.45) (Material (Dielectric 1.5)))
    ]

main :: IO ()
main = do
    saveImage 
    -- testCode

testCode = do
    let g = mkStdGen 22
    print "test Code"

saveImage = do
    -- writeImage 1800
    -- writeImage 800
    -- writeImage 384 100 world
    writeImage 204 40 world
    -- writeImage 100 10 world
    -- writeImage 20 5 world
    -- writeImage 4 samplePerPixels world
