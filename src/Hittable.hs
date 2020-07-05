{-# LANGUAGE ExistentialQuantification #-}

module Hittable where

import           Data.Maybe
import           GHC.OldList (find)
import System.Random

import           BaseVec
import           Ray
import           Vec
import Colors
import Samplings


data HitRecord = HitRecord {
    p :: Point3,
    normal :: Vec3,
    mat :: Material,
    t :: Double,
    front_face :: Bool
}

class Hittable a where
    hit :: a -> Ray -> Double -> Double -> Maybe HitRecord


data Sphere = Sphere {
    center :: Point3,
    radius :: Double,
    material :: Material
}

instance Hittable Sphere where
    hit (Sphere center radius material) ray@(Ray origin direction) tmin tmax = record
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
                        return $ HitRecord point normal material t front_face
            record = if discriminant > 0 then recordFn $ sqrt discriminant else Nothing

newtype HittableList a = HittableList [a]

instance Hittable a => Hittable (HittableList a) where
    hit (HittableList items) ray tmin tmax = record
        where
            reduce i (r, max) = fromMaybe (r, max) m
                where
                    m = do
                        rec <- hit i ray tmin max
                        return (Just rec, t rec)
            record = fst $ foldr reduce (Nothing, tmax) items

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v - (n .* (2 * (dot v n)))

data Material = forall a. Scatterable a => Material a

class Scatterable a where
    scatter :: RandomGen g => a -> Ray -> HitRecord -> g -> (Maybe (Ray, ColorVec), g)

data Lambertian = Lambertian ColorVec
data Metal = Metal ColorVec Double
data Dielectric = Dielectric

instance Scatterable Lambertian where
    scatter (Lambertian color) _ray (HitRecord p normal _ _ _) g = (Just (scattered, color), g1)
        where
            (sampled, g1) = sampleUnitVector g
            scatter_direction = normal + sampled
            scattered = Ray p scatter_direction

instance Scatterable Metal where
    scatter (Metal color fuzz) (Ray _ direction) (HitRecord p normal _ _ _) g = if dot reflected normal > 0
                                                                              then (Just (scattered, color), g1)
                                                                              else (Nothing, g1)
        where
            (sampled, g1) = samplePointInSphere g 1
            reflected = reflect (unit direction) normal
            scattered = Ray p (reflected + sampled .* (min fuzz 1.0))

--
-- instance Scatterable Dielectric where
--     scatter m r h = undefined
--
-- testMaterials :: [Material]
-- testMaterials = [Material Lambertian, Material Metal, Material Dielectric]
