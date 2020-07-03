
{-# LANGUAGE NamedFieldPuns #-}

module Image where

import           Data.Maybe
import           Text.Printf (printf)

import           Hittable
import           Ray
import           Vec
import           BaseVec

type Color = BaseVec Int
type FractionalColor = BaseVec Double

color :: Color -> String
color (BaseVec [r,g,b]) = printf "%3d %3d %3d\n" r g b

toColor :: FractionalColor -> Color
toColor = fmap (floor . (255.99 *))

fromColor :: Color -> Vec3
fromColor = fmap fromIntegral

fromVec :: Vec3 -> Color
fromVec = fmap floor

data Image = Image {
        width  :: Int,
        height :: Int,
        pixels :: [[Color]]
    } deriving (Show)

rayColor ray@(Ray origin direction) world = toColor $ fromMaybe default_color $ do
                                                (HitRecord _ normal _ _ ) <- h
                                                return $ (normal .+ 1) .* 0.5
    where
        h = hit world ray 0 100000000000
        unit_direction = unit direction
        t = 0.5 * (y (unit direction) + 1.0)
        default_color = toVec 1 1 1 .* (1.0 - t) + toVec 0.5 0.7 1.0 .* t

hitSphere center radius (Ray origin direction) = if discriminant < 0
                                                    then -1.0
                                                    else (-half_b - sqrt discriminant) / a
    where
        oc = origin - center
        a = len2 direction
        half_b = dot oc direction
        c = len2 oc - radius^2
        discriminant = half_b^2 - a*c
