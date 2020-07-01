
{-# LANGUAGE NamedFieldPuns #-}

module Image where

import           Data.Maybe
import           Text.Printf (printf)

import           Hittable
import           Ray
import           Vec

newtype Color = Color (Int, Int, Int) deriving(Show)

color :: Color -> String
color (Color(r,g,b)) = printf "%3d %3d %3d\n" r g b

toColor Vec3 {x, y, z} = Color (r, g, b)
    where
        r = floor $ 255.99 * x
        g = floor $ 255.99 * y
        b = floor $ 255.99 * z

fromColor (Color(r, g, b)) = Vec3 { x = fromIntegral r, y = fromIntegral g,  z = fromIntegral b }
fromVec Vec3{x, y, z} = Color (floor x, floor y, floor z)

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

rayColor ray@(Ray origin direction) world = toColor $ fromMaybe default_color $ do
                                                (HitRecord _ normal _ _ ) <- h
                                                return $ apply (*0.5) $ apply (+1) normal
    where
        h = hit world ray 0 100000000000
        unit_direction = unit direction
        t = 0.5 * (y (unit direction) + 1.0)
        default_color = apply (*(1.0 - t)) (toVec 1 1 1) + apply (*t) (toVec 0.5 0.7 1.0)

hitSphere center radius (Ray origin direction) = if discriminant < 0
                                                    then -1.0
                                                    else (-half_b - sqrt discriminant) / a
    where
        oc = origin - center
        a = len2 direction
        half_b = dot oc direction
        c = len2 oc - radius^2
        discriminant = half_b^2 - a*c
