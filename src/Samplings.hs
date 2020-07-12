{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeFamilies        #-}

module Samplings(
    sampleFraction,
    sampleFractions,
    sampleBetween,
    sampleBetweens,
    samplePoint,
    samplePoints,
    samplePointBetween,
    samplePointInSphere,
    samplePointInHemisphere,
    sampleUnitVector,
    sampleVecInUnitDisk,
) where

import           Data.List     (foldl')
import           Data.Maybe    (fromJust, isJust)
import           System.Random (RandomGen, randomR)

import           Vec           (Point3, Vec (Vec), Vec3, dot, lenSquared)

sampleFraction :: RandomGen g => g -> (Double, g)
sampleFraction g = randomR (0, 1) g

sampleFractions :: RandomGen g => g -> Int -> ([Double], g)
sampleFractions g n = foldl' gen ([], g) [1..n]
    where
        gen (xs, g1) _ = (x:xs, g2)
            where
                (x, g2) = sampleFraction g1

sampleBetween :: RandomGen g => g -> Double -> Double -> (Double, g)
sampleBetween g minVal maxVal = (minVal + r * (maxVal - minVal), g')
    where
        (r, g') = sampleFraction g

sampleBetweens :: RandomGen g => g -> Double -> Double -> Int -> ([Double], g)
sampleBetweens g minVal maxVal n = (fmap (\r -> minVal + r * (maxVal - minVal)) xs, g')
    where
        (xs, g') = sampleFractions g n

samplePoint :: RandomGen g => g -> (Point3, g)
samplePoint g = (Vec (xyz !! 0) (xyz !! 1) (xyz !! 2), g')
    where
        (xyz, g') = sampleFractions g 3

samplePoints :: RandomGen g => g -> Int -> ([Point3], g)
samplePoints g n = foldl' gen ([], g) [1..n]
    where
        gen (xs, g1) _ = (x:xs, g2)
            where
                (x, g2) = samplePoint g1

samplePointBetween :: RandomGen g => g -> Double -> Double -> (Point3, g)
samplePointBetween g a b = (Vec (xyz !! 0) (xyz !! 1) (xyz !! 2), g')
    where
        (xyz, g') = sampleBetweens g a b 3

samplePointInSphere :: RandomGen g => g -> Double -> (Point3, g)
samplePointInSphere g radius = fromJust (find g)
    where
        find g1 = p
            where
                (maybeP, g2) = maybePoint g1
                p = if isJust maybeP
                       then Just (fromJust maybeP, g2)
                        else find g2

        maybePoint g1 = (maybeP, g2)
            where
                (p, g2) = samplePointBetween g1 (-radius) radius
                maybeP = if (lenSquared p) < radius then Just p else Nothing

sampleUnitVector :: RandomGen g => g -> (Vec3, g)
sampleUnitVector g = (Vec (r * cos a) (r * sin a) z, g2)
    where
        (a, g1) = sampleBetween g 0 (2 * pi)
        (z, g2) = sampleBetween g1 (-1) 1
        r = sqrt (1 - z * z)


samplePointInHemisphere :: RandomGen g => g -> Double -> Vec3 -> (Point3, g)
samplePointInHemisphere g radius normal = if dot p normal > 0.0 then (p, g1) else (-p, g1)
    where
        (p, g1) = samplePointInSphere g radius

sampleVecInUnitDisk :: RandomGen g => g -> (Vec3, g)
sampleVecInUnitDisk g = fromJust (find g)
    where
        find g1 = p
            where
                (maybeV, g2) = maybeVec g1
                p = if isJust maybeV
                       then Just (fromJust maybeV, g2)
                        else find g2

        maybeVec g1 = (maybeV, g3)
            where
                (x, g2) = sampleBetween g1 (-1) 1
                (y, g3) = sampleBetween g2 (-1) 1
                v = Vec x y 0
                maybeV = if (lenSquared v) < 1.0 then Just v else Nothing
