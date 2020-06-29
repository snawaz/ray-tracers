

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Data.List   (intercalate)
import           System.IO   (IOMode (AppendMode, WriteMode), hFlush, hPutStrLn,
                              stdout, withFile)
import           Text.Printf (printf)

newtype Color = Color (Int, Int, Int) deriving(Show)

color :: Color -> String
color (Color(r,g,b)) = printf "%3d %3d %3d" r g b

data Image = Image {
        width  :: Int,
        height :: Int,
        pixels :: [[Color]]
    } deriving (Show)

createRandomImage :: Int -> Int -> Image
createRandomImage width height = Image {
        width = width,
        height = height,
        pixels = do
                j <- reverse [0..height-1]
                let makeColor i j = Color(r, g, b)
                        where
                            r = floor $ 255.99 * fromIntegral i / fromIntegral (width - 1)
                            g = floor $ 255.99 * fromIntegral j / fromIntegral (height - 1)
                            b = floor $ 255.99 * 0.25
                return [makeColor i j | i <- [0..width-1]]
        }

main :: IO ()
main = do
    -- let image = createRandomImage 4 6-- 256 256
    let image = createRandomImage 256 256
    putStrLn "P3"
    putStrLn $ show (width image) ++ " " ++ show (height image)
    print 255
    putStrLn $ unlines [ intercalate "    " [color c | c <- row] | row <- pixels image]
