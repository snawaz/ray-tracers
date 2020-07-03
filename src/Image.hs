
{-# LANGUAGE NamedFieldPuns #-}

module Image where

import           Data.Maybe
import           Text.Printf (printf)

import           BaseVec
import           Hittable
import           Ray
import           Vec

type Color = BaseVec Int
type FractionalColor = BaseVec Double
newtype SampledColor = SampledColor (Int, BaseVec Double)

instance Num SampledColor where
    (SampledColor(m, v1)) + (SampledColor (n, v2)) = SampledColor(n, v1 + v2)
    (SampledColor(m, v1)) * (SampledColor (n, v2)) = SampledColor(n, v1 * v2)
    (SampledColor(m, v1)) - (SampledColor (n, v2)) = SampledColor(n, v1 - v2)
    abs (SampledColor(n, v)) = SampledColor(n, abs v)
    signum (SampledColor(n, v)) = SampledColor(n, signum v)
    fromInteger n = SampledColor(fromInteger n, fromInteger 0)

color :: Color -> String
color (BaseVec [r,g,b]) = printf "%3d %3d %3d\n" r g b

-- class From a where
--    from :: a -> Color

--instance ToColor

toColor :: FractionalColor -> Color
toColor = fmap (floor . (255.99 *))

fromSampledColor :: SampledColor -> Color
fromSampledColor (SampledColor(n, v)) = fmap floor $ v ./ fromIntegral n

toSampledColor :: Int -> Color -> SampledColor
toSampledColor n color = SampledColor(n, fmap fromIntegral color)

data Image = Image {
        width  :: Int,
        height :: Int,
        pixels :: [[Color]]
    } deriving (Show)

rayColor :: (Hittable a) => Ray -> a -> Color
rayColor ray@(Ray origin direction) world = toColor $ fromMaybe default_color $ do
                                                (HitRecord _ normal _ _ ) <- h
                                                return $ (normal .+ 1) .* 0.5
    where
        h = hit world ray 0 100000000000
        unit_direction = unit direction
        t = 0.5 * (y (unit direction) + 1.0)
        default_color = vec 1 1 1 .* (1.0 - t) + vec 0.5 0.7 1.0 .* t

hitSphere center radius (Ray origin direction) = if discriminant < 0
                                                    then -1.0
                                                    else (-half_b - sqrt discriminant) / a
    where
        oc = origin - center
        a = len2 direction
        half_b = dot oc direction
        c = len2 oc - radius^2
        discriminant = half_b^2 - a*c
