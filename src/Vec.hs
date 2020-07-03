
module Vec where

import BaseVec
import           Utils

type Vec3 = BaseVec Double

type Point3 = Vec3

len2 :: Vec3 -> Double
len2 v = dot v v

len :: Vec3 -> Double
len = sqrt . len2

unit v = v ./ len v
