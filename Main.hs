

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Data.List (intercalate)
import           System.IO (IOMode (WriteMode), hFlush, hPutStrLn, withFile)

import           Hittable
import           Image
import           Ray
import           Vec
import           Utils
import Camera
import System.Random

samplePerPixels = 100

world = HittableList [
        (Sphere (toVec 0 0 (-1)) 0.5),
        (Sphere (toVec 0 (-100.5) (-1)) 100)
    ]

main :: IO ()
main = do
    -- writeImage 1800
    writeImage 800
    -- testTools

writeImage imageWidth = do
    let gen = mkStdGen 22
    let randList = (randomRs (0,1) gen) :: [Double]
    let image = createImage imageWidth (floor (fromIntegral imageWidth / aspectRatio)) randList
    putStrLn "P3"
    putStrLn $ show (width image) ++ " " ++ show (height image)
    print 255
    putStrLn $ unlines [ intercalate "    " [color c | c <- row] | row <- pixels image]

testTools = do
    let gen = mkStdGen 22
    let r = (randomRs (0,1) gen) :: [Double]
    print $ take 5 r
    print $ take 6 r

createImage :: Int -> Int -> [Double] -> Image
createImage width height randList = Image {
    width = width,
    height = height,
    pixels = do
        j <- reverse [0..height - 1]
        return $ do
            i <- [0..width - 1]
            let rands = take samplePerPixels randList
            let color = foldr (+) (toVec 0 0 0) $ fmap (randomRayColor i j world) rands
            return $ fromVec $ apply (/fromIntegral samplePerPixels) color
    }
        where
            randomRayColor i j world rand = fromColor $ rayColor (ray camera u v) world
                where
                    u = (fromIntegral i + rand) / fromIntegral (width - 1)
                    v = (fromIntegral j + rand) / fromIntegral (height - 1)
