
{-# LANGUAGE NamedFieldPuns #-}

module Image where

import           Text.Printf (printf)

import Vec
import Ray

newtype Color = Color (Int, Int, Int) deriving(Show)

color :: Color -> String
color (Color(r,g,b)) = printf "%3d %3d %3d\n" r g b

toColor Vec3 {x, y, z} = Color (r, g, b)
    where
        r = floor $ 255.99 * x
        g = floor $ 255.99 * y
        b = floor $ 255.99 * z

data Image = Image {
        width  :: Int,
        height :: Int,
        pixels :: [[Color]]
    } deriving (Show)

pixelColor :: Int -> Int -> Int -> Int -> Color
pixelColor i j width height = Color(r, g, b)
    where
        r = floor $ 255.99 * fromIntegral i / fromIntegral (width - 1)
        g = floor $ 255.99 * fromIntegral j / fromIntegral (height - 1)
        b = floor $ 255.99 * 0.25

rayColor (Ray origin direction) = toColor $ if h > 0.0
                                               then apply (*0.5) $ apply (+1) $ unit $ pointAt (Ray origin direction) h - toVec 0 0 (-1)
                                               else p1 + p2
    where
        h = hitSphere (toVec 0 0 (-1)) 0.5 (Ray origin direction)
        unit_direction = unit direction
        t = 0.5 * (y (unit direction) + 1.0)
        p1 = apply (*(1.0 - t)) (toVec 1 1 1)
        p2 = apply (*t) (toVec 0.5 0.7 1.0)

hitSphere center radius (Ray origin direction) = if discriminant < 0
                                                    then -1.0
                                                    else (-b - sqrt discriminant) / (2.0 * a)
    where
        oc = origin - center
        a = len2 direction
        b = 2.0 * dot oc direction
        c = len2 oc - radius^2
        discriminant = b^2 - 4*a*c
