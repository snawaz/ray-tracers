{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# OPTIONS -Wno-unused-top-binds #-}

module Hittable(
    HitRecord (HitRecord),
    Hittable(hit),
    HittableList (HittableList),
    toHittableList,
    Sphere (Sphere),
    Material (Material),
    Lambertian (Lambertian), Metal (Metal), Dielectric (Dielectric),
    Scatterable, scatter
) where

import           Data.Array      (Array, bounds, listArray)
import           Data.Array.Base (unsafeAt)
import           Data.Maybe      (fromMaybe)
import           System.Random   (RandomGen)

import           Colors          (ColorVec, toColor)
import           Ray             (Ray (Ray), pointAt)
import           Samplings       (sampleFraction, samplePointInSphere, sampleUnitVector)
import           Vec             (Point3, Vec3, dot, lenSquared, one, unit, (.*), (./))


data HitRecord = HitRecord {
    getP        :: !Point3,
    getNormal   :: !Vec3,
    getmat      :: !Material,
    getT        :: !Double,
    isFrontFace :: !Bool
}

class Hittable a where
    hit :: a -> Ray -> Double -> Double -> Maybe HitRecord

data Sphere = Sphere {
    getCenter   :: !Point3,
    getRadius   :: !Double,
    getMaterial :: !Material
}

instance Hittable Sphere where
    {-# INLINE hit #-}
    hit (Sphere center radius material) ray@(Ray origin direction) tmin tmax = record
        where
         oc = origin - center
         a = lenSquared direction
         half_b = dot oc direction
         c = lenSquared oc - radius * radius
         discriminant = half_b * half_b - a * c
         {-# INLINE recordFn #-}
         recordFn root = r
             where
                 mkRecord t' =  HitRecord point normal material t' front_face
                     where
                         point = pointAt ray t'
                         outward_normal = (point - center) ./ radius
                         front_face = dot direction outward_normal < 0
                         normal = if front_face then outward_normal else -outward_normal
                 t1 = (-half_b - root)/a
                 t2 = (-half_b + root)/a
                 r = if t1 < tmax && t1 > tmin
                        then Just (mkRecord t1)
                        else if t2 < tmax && t2 > tmin
                             then Just (mkRecord t2)
                             else Nothing
         record =  if discriminant > 0 then recordFn $ sqrt discriminant else Nothing

newtype HittableList a = HittableList (Array Int a)

toHittableList :: Hittable a => [a] -> HittableList a
toHittableList objects = HittableList $ listArray (1, length objects) objects

-- https://gitlab.haskell.org/ghc/ghc/-/issues/8763
{-# INLINE loop_hit #-}
loop_hit :: (a -> Int -> a) -> a -> Int -> a
loop_hit f v n = go 0 v
  where
      go !i arg | i == n = arg
                | otherwise = go (i+1) (f arg i)

instance Hittable a => Hittable (HittableList a) where
    hit (HittableList items) ray tmin tmax = record
        where
            (s, e) = bounds items
            record = fst $ loop_hit reduce (Nothing, tmax) (e-s+1)
            reduce (current_rec, current_max) i = fromMaybe (current_rec, current_max) m
                where
                    m = do
                        rec <- hit (unsafeAt items i) ray tmin current_max
                        return (Just rec, getT rec)


reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v - (n .* (2 * (dot v n)))

data Material = forall a. Scatterable a => Material a

class Scatterable a where
    scatter :: RandomGen g => a -> Ray -> HitRecord -> g -> (Maybe (Ray, ColorVec), g)

data Lambertian = Lambertian ColorVec
data Metal = Metal ColorVec Double
data Dielectric = Dielectric Double

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

refract ::  Vec3 -> Vec3 -> Double -> Vec3
refract unitRay normal etai_over_etat = r_out_perp + r_out_perp
    where
        cos_theta = dot (-unitRay) normal
        r_out_parallel = (unitRay + normal .* cos_theta) .* etai_over_etat
        r_out_perp = negate $ normal .* sqrt (1.0 - lenSquared r_out_parallel)

schlick :: Double -> Double -> Double
schlick cosine refIdx = r0 + (1 - r0) * (1-cosine) ^ 5
    where
        r0 = ((1-refIdx) / (1+refIdx)) ^ 2

instance Scatterable Dielectric where
    scatter (Dielectric refIdx) (Ray _ direction) rec g = (Just (scattered, attenuation), g1)
        where
            attenuation = toColor (one :: Vec3)
            etai_over_etat = if isFrontFace rec then 1.0 / refIdx else refIdx
            cos_theta = min (dot (negate $ unit direction) (getNormal rec)) 1.0
            sin_theta = sqrt (1.0 - cos_theta * cos_theta)
            reflect_probability = schlick cos_theta etai_over_etat
            (sampled, g1) = sampleFraction g
            scatter_direction = if etai_over_etat * sin_theta > 1.0 || reflect_probability > sampled
                                   then reflect (unit direction) (getNormal rec)
                                   else refract (unit direction) (getNormal rec) etai_over_etat
            scattered = Ray (getP rec) scatter_direction
