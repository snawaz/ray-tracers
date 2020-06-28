

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import System.IO                  (IOMode (AppendMode, WriteMode), hFlush, hPutStrLn, stdout, withFile)
--import Data.Ratio
import Text.Printf        (printf)
import Data.List (intercalate)

newtype Color = Color (Int, Int, Int) deriving(Show)

color :: Color -> String
color (Color(r,g,b)) = printf "%3d %3d %3d" r g b

data Image = Image {
        width :: Int,
        height :: Int,
        pixels :: [[Color]]
    } deriving (Show)


createRandomImage :: Int -> Int -> Image
createRandomImage width height = Image {
        width = width,
        height = height,
        pixels = do
                j <- reverse [0..height-1]
                let makeColor i j = Color(floor r :: Int, floor g :: Int, floor b :: Int)
                        where
                            r = (255.99 :: Double) * i / fromIntegral (width - 1)
                            g = (255.99 :: Double) * j / fromIntegral (height - 1)
                            b = (255.99 :: Double) * 0.25
                return [makeColor (fromIntegral  i) (fromIntegral  j) | i <- [0..width-1]]
        } 

main :: IO ()
main = do
    -- let image = createRandomImage 4 6-- 256 256 
    let image = createRandomImage 256 256 
    putStrLn "P3"
    putStrLn $ show (width image) ++ " " ++ show (height image) 
    print 255
    -- putStrLn $ unlines [color c | row <- pixels image, c <- row] 
    putStrLn $ unlines [ intercalate "    " [color c | c <- row] | row <- pixels image] 
