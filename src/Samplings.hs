{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeFamilies        #-}

module Samplings(
    sampleFraction,
    sampleFraction2,
    sampleFraction3,
    sampleBetween,
    sampleBetween2,
    sampleBetween3,
    samplePoint,
    samplePoint2,
    samplePointBetween,
    samplePointInSphere,
    samplePointInHemisphere,
    sampleUnitVector,
    sampleVecInUnitDisk,
) where

import           Data.Maybe    (fromJust, isJust)
import           System.Random (RandomGen, randomR)

import           Vec           (Point3, Vec (Vec), Vec3, dot, lenSquared)

--------- Functions returning Double and Doubles ----------

{-# INLINE sampleFraction #-}
sampleFraction :: RandomGen g => g -> (Double, g)
sampleFraction g = randomR (0, 1) g

{-# INLINE sampleFraction2 #-}
sampleFraction2 :: RandomGen g => g -> ((Double, Double), g)
sampleFraction2 g = ((x, y), g2)
    where
        (x, g1) = sampleFraction g
        (y, g2) = sampleFraction g1

{-# INLINE sampleFraction3 #-}
sampleFraction3 :: RandomGen g => g -> ((Double, Double, Double), g)
sampleFraction3 g = ((x, y, z), g3)
    where
        (x, g1) = sampleFraction g
        (y, g2) = sampleFraction g1
        (z, g3) = sampleFraction g2

{-# INLINE sampleBetween #-}
sampleBetween :: RandomGen g => g -> Double -> Double -> (Double, g)
sampleBetween g minVal maxVal = (minVal + r * (maxVal - minVal), g')
    where
        (r, g') = sampleFraction g

{-# INLINE sampleBetween2 #-}
sampleBetween2 :: RandomGen g => g -> Double -> Double -> ((Double, Double), g)
sampleBetween2 g minVal maxVal =  ((x, y), g2)
    where
        (x, g1) = sampleBetween g minVal maxVal
        (y, g2) = sampleBetween g1 minVal maxVal

{-# INLINE sampleBetween3 #-}
sampleBetween3 :: RandomGen g => g -> Double -> Double -> ((Double, Double, Double), g)
sampleBetween3 g minVal maxVal =  ((x, y, z), g3)
    where
        (x, g1) = sampleBetween g minVal maxVal
        (y, g2) = sampleBetween g1 minVal maxVal
        (z, g3) = sampleBetween g2 minVal maxVal

-------- Functions returning Point3 and Point3s ----------------

{-# INLINE samplePointBetween #-}
samplePointBetween :: RandomGen g => g -> Double -> Double -> (Point3, g)
samplePointBetween g a b = (Vec x y z, g1)
    where
        ((x, y, z), g1) = sampleBetween3 g a b

{-# INLINE samplePoint #-}
samplePoint :: RandomGen g => g -> (Point3, g)
samplePoint g = samplePointBetween g 0 1

{-# INLINE samplePoint2 #-}
samplePoint2 :: RandomGen g => g -> ((Point3, Point3), g)
samplePoint2 g = ((p1, p2), g2)
    where
        (p1, g1) = samplePoint g
        (p2, g2) = samplePoint g1

{-# INLINE samplePointInSphere #-}
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

{-# INLINE sampleUnitVector #-}
sampleUnitVector :: RandomGen g => g -> (Vec3, g)
sampleUnitVector g = (Vec (r * cos a) (r * sin a) z, g2)
    where
        (a, g1) = sampleBetween g 0 (2 * pi)
        (z, g2) = sampleBetween g1 (-1) 1
        r = sqrt (1 - z * z)


{-# INLINE samplePointInHemisphere #-}
samplePointInHemisphere :: RandomGen g => g -> Double -> Vec3 -> (Point3, g)
samplePointInHemisphere g radius normal = if dot p normal > 0.0 then (p, g1) else (-p, g1)
    where
        (p, g1) = samplePointInSphere g radius

{-# INLINE sampleVecInUnitDisk #-}
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
