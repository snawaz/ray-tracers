
{-# LANGUAGE NamedFieldPuns #-}

module Vec where

import BaseVec
import           Utils

-- data Vec3 = Vec3 {
--         x :: Double,
--         y :: Double,
--         z :: Double
--     } deriving (Show)

type Vec3 = BaseVec Double

type Point3 = Vec3

toVec :: Double -> Double -> Double -> Vec3
-- toVec x y z = Vec3 { x, y, z }
toVec = vec 

-- :toList :: Vec3 -> [Double]
-- :toList Vec3{x, y, z} = [x, y, z]
-- :
-- :fromList :: [Double] -> Vec3
-- :fromList [x, y, z] = toVec x y z
-- :
-- :instance Num Vec3 where
-- :    v1 + v2 = fromList $ zipWith (+) (toList v1) (toList v2)
-- :    v1 * v2 = fromList $ zipWith (-) a b
-- :                            where
-- :                                l1 = toList v1
-- :                                l2 = toList v2
-- :                                a = zipWith (*) (rotate 1 l1) (rotate 2 l2)
-- :                                b = zipWith (*) (rotate 2 l1) (rotate 1 l2)
-- :    v1 - v2 = fromList $ zipWith (-) (toList v1) (toList v2)
-- :    abs    v = fromList $ fmap abs (toList v)
-- :    signum v = fromList $ fmap signum (toList v)
-- :    fromInteger n = fromList $ replicate 3 (fromInteger n)

-- dot :: Vec3 -> Vec3 -> Double
-- dot v1 v2 = sum $ zipWith (*) (toList v1) (toList v2)

len2 :: Vec3 -> Double
len2 v = dot v v

len :: Vec3 -> Double
len = sqrt . len2

unit v = v ./ len v
