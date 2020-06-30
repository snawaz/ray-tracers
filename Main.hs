

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Data.List (intercalate)
import           System.IO (IOMode (WriteMode), hFlush, hPutStrLn, withFile)

import           Image
import           Vec
import           Ray

aspectRatio = 16.0 / 9.0;
viewportHeight = 2.0
viewportWidth = aspectRatio * viewportHeight
focalLength = 1.0

origin = toVec 0 0 0
horizontal = toVec viewportWidth 0 0
vertical = toVec 0 viewportHeight 0
loweLeftCorner = origin - (apply (/2) horizontal) - (apply (/2) vertical) - toVec 0 0 focalLength

main :: IO ()
main = do
    writeImage
    -- testTools

createImage :: Int -> Int -> Image
createImage width height = Image {
    width = width,
    height = height,
    pixels = do
        j <- reverse [0..height - 1]
        return $ do
            i <- [0..width - 1]
            let u = fromIntegral i / fromIntegral (width - 1)
            let v = fromIntegral j / fromIntegral (height - 1)
            let r = Ray origin (loweLeftCorner + (apply (*u) horizontal) + (apply (*v) vertical) - origin)
            return $ rayColor r
    }

testTools = do
    print origin
    print horizontal
    print vertical
    print loweLeftCorner
    -- print $ dot (toVec 1 2 3) (toVec 3 4 5)
    -- print $ apply (*10) (toVec 1 2 3)
    -- print $ apply (/10) (toVec 1 2 3)
    -- print $ (fromList [5.0, 7.0, 1.0]) + (fromList [4.0, 4.0, 3.0])
    -- print $ (fromList [5.0, 7.0, 1.0]) - (fromList [4.0, 4.0, 3.0])
    -- print $ (fromList [5.0, 7.0, 1.0]) * (fromList [4.0, 4.0, 3.0])
    -- print $ len2 (toVec 1 2 3)
    -- print $ len (toVec 1 2 3)

writeImage = do
    -- let image = createRandomImage 2 3-- 256 256
    let image = createImage 384 $ floor (384 / aspectRatio)
    -- let image = createRandomImage 256 256
    putStrLn "P3"
    putStrLn $ show (width image) ++ " " ++ show (height image)
    print 255
    putStrLn $ unlines [ intercalate "    " [color c | c <- row] | row <- pixels image]

createRandomImage :: Int -> Int -> Image
createRandomImage width height = Image {
        width = width,
        height = height,
        pixels = do
                j <- reverse [0..height-1]
                return $ do
                    i <- [0..width-1]
                    return $ pixelColor i j width height
        }
