
module Hittable where

import           Data.Maybe
import           GHC.OldList (find)

import           Ray
import           Vec
import           BaseVec

data HitRecord = HitRecord Point3 Vec3 Double Bool

class Hittable a where
    hit :: a -> Ray -> Double -> Double -> Maybe HitRecord


data Sphere = Sphere Point3 Double

instance Hittable Sphere where
    hit (Sphere center radius) ray@(Ray origin direction) tmin tmax = record
        where
            oc = origin - center
            a = len2 direction
            half_b = dot oc direction
            c = len2 oc - radius^2
            discriminant = half_b^2 - a*c
            recordFn root = r
                where
                    ts = [(-half_b - root)/a, (-half_b + root)/a]
                    r = do
                        t <- find (\t -> t < tmax && t > tmin) ts
                        let point = pointAt ray t
                        let outward_normal = (point - center) ./ radius
                        let front_face = dot direction outward_normal < 0
                        let normal = if front_face then outward_normal else -outward_normal
                        return $ HitRecord point normal t front_face
            record = if discriminant > 0 then recordFn $ sqrt discriminant else Nothing

newtype HittableList a = HittableList [a]

instance Hittable a => Hittable (HittableList a) where
    hit (HittableList items) ray tmin tmax = record
        where
            reduce i (r, max) = fromMaybe (r, max) m
                where
                    m = do
                        (HitRecord p v t f) <- hit i ray tmin max
                        return (Just (HitRecord p v t f), t)
            record = fst $ foldr reduce (Nothing, tmax) items
