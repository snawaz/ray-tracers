

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           System.Random
import Control.Exception (evaluate)
import           Data.List     ( foldl')
import Control.DeepSeq
import           System.Environment

import           BaseVec
import           Hittable
import           Image
import           Samplings
import           Scenes
import           Utils

r = cos (pi/4)
-- world = HittableList [
--         -- (Sphere (vec (-r) 0 (-1)) r (Material (Lambertian (vec 0 0 1)))),
--         -- (Sphere (vec r 0 (-1)) r (Material (Lambertian (vec 1 0 0))))
--         (Sphere (vec 0 0 (-1)) 0.5 (Material (Lambertian (vec 0.1 0.2 0.5)))),
--         (Sphere (vec 0 (-100.5) (-1)) 100 (Material (Lambertian (vec 0.8 0.8 0.0)))),
--         (Sphere (vec 1 0 (-1)) 0.5 (Material (Metal (vec 0.8 0.6 0.2) 0.3))),
--         (Sphere (vec (-1) 0 (-1)) 0.5 (Material (Dielectric 1.5))),
--         (Sphere (vec (-1) 0 (-1)) (-0.45) (Material (Dielectric 1.5)))
--     ]

main :: IO ()
main = do
    -- testCode
    args <- getArgs
    let [width, samplePerPixels, depth] = fmap read args
    saveImage width samplePerPixels depth

saveImage :: Int -> Int -> Int -> IO ()
saveImage width samplePerPixels depth = do
    printCurrentTime $ \now -> "Start time => " ++ now 
    let scene = randomScene 23
    (_, elapsed) <- getSecondsElapsed $ writeImage width samplePerPixels scene depth
    printCurrentTime $ \now -> "End time   => " ++ now 
    putStrLn $ "\n\033[32mTOTAL TIME ELAPSED: " ++ show elapsed ++ "\033[0m\n"

testMs ms1 = do
    (m1, me1) <- getMicrosecondsElapsed $ evaluate $ force $ foldl' (\a b -> a + b) (head ms1) ms1
    putStrLn $ "ms1 summed: " ++ show m1 ++ ", time: " ++ show me1

testVs vs1 = do
    (v1, ve1) <- getMicrosecondsElapsed $ evaluate $ force $ foldl' (\a b -> a + b) (head vs1) vs1
    putStrLn $ "vs1 summed: " ++ show v1 ++ ", time: " ++ show ve1

testCode = do
    let g = mkStdGen 22
    let (xs, _) = samplePoints g 1000
    let vs = foldl' (\a _ -> a ++ xs) xs [1..100] 
    let ms = fmap (\(BaseVec x y z) -> MyV x y z) vs
    
    (vs1, vse1) <- getMicrosecondsElapsed $ evaluate $ force vs
    (ms1, mse1) <- getMicrosecondsElapsed $ evaluate $ force ms
    putStrLn $ "vs evaluated: " ++ show vse1
    putStrLn $ "vs evaluated: " ++ show mse1

    testMs ms1
    testMs ms1
    testVs vs1
    testVs vs1

    testVs vs1
    testVs vs1
    testMs ms1
    testMs ms1

    putStrLn "Done"
