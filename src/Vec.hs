
module Vec where

import           BaseVec
import           Utils

type Vec3 = BaseVec Double

type Point3 = Vec3

{-# INLINE len2 #-}
len2 :: Vec3 -> Double
len2 v = dot v v

{-# INLINE len #-}
len :: Vec3 -> Double
len = sqrt . len2

{-# INLINE unit #-}
unit :: Vec3 -> Vec3
unit v = v ./ len v
