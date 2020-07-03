

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Data.List     (intercalate)
import           System.IO     (IOMode (WriteMode), hFlush, hPutStrLn, withFile)

import           BaseVec
import           Camera
import           Hittable
import           Image
import           Ray
import           System.Random
import           Utils
import           Vec

samplePerPixels = 100

world = HittableList [
        (Sphere (vec 0 0 (-1)) 0.5),
        (Sphere (vec 0 (-100.5) (-1)) 100)
    ]

main :: IO ()
main = do
    -- writeImage 1800
    -- writeImage 800
    writeImage 384
    -- testTools

writeImage imageWidth = do
    let gen = mkStdGen 22
    let randList = (randomRs (0,1) gen) :: [Double]
    let image = createImage imageWidth (floor (fromIntegral imageWidth / aspectRatio)) randList
    putStrLn "P3"
    putStrLn $ show (width image) ++ " " ++ show (height image)
    print 255
    putStrLn $ unlines [ intercalate "    " [color c | c <- row] | row <- pixels image]


kolor :: Kolor -> Kolor
kolor a = a

testTools = do
    let gen = mkStdGen 22
    let r = (randomRs (0,1) gen) :: [Double]
    print $ take 5 r
    print $ take 6 r
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

createImage :: Int -> Int -> [Double] -> Image
createImage width height randList = Image {
    width = width,
    height = height,
    pixels = do
        j <- reverse [0..height - 1]
        return $ do
            i <- [0..width - 1]
            let rands = take samplePerPixels randList
            let sampledColor = foldr (+) (SampledColor(samplePerPixels, vec 0 0 0)) $ fmap (randomRayColor i j world) rands
            return $ fromSampledColor sampledColor
    }
        where
            randomRayColor i j world rand = toSampledColor samplePerPixels $ rayColor (ray camera u v) world
                where
                    u = (fromIntegral i + rand) / fromIntegral (width - 1)
                    v = (fromIntegral j + rand) / fromIntegral (height - 1)
