
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Hittable(
    HitRecord (HitRecord),
    Hittable(hit),
    HittableList (HittableList), 
    toHittableList, printHistableList,
    Sphere (Sphere),
    Material (Material),
    Lambertian (Lambertian), Metal (Metal), Dielectric (Dielectric),
    Scatterable, scatter
) where

import           Data.List     (foldl')
import           Data.Maybe    (fromMaybe)
import           GHC.OldList   (find)
import           System.Random (RandomGen)
import           Data.Array (Array, elems, bounds, listArray)
import           Data.Array.Base (unsafeAt)

import           Colors        (ColorVec)
import           Ray           (Ray (Ray), pointAt)
import           Samplings     (sampleFraction, samplePointInSphere, sampleUnitVector)
import           Vec           (Point3, Vec3, dot, lenSquared, one, unit, (.*), (./))


data HitRecord = HitRecord {
    p          :: !Point3,
    normal     :: !Vec3,
    mat        :: !Material,
    t          :: !Double,
    front_face :: !Bool
}

class Hittable a where
    hit :: a -> Ray -> Double -> Double -> Maybe HitRecord

data Sphere = Sphere {
    center   :: !Point3,
    radius   :: !Double,
    material :: !Material
}

{-# INLINE hit_one #-}
hit_one center radius origin direction = (oc, a, half_b, c, discriminant)
    where 
        oc = origin - center
        a = lenSquared direction
        half_b = dot oc direction
        c = lenSquared oc - radius * radius
        discriminant = half_b * half_b - a * c


{-# INLINE hit_two #-}
hit_two center radius material ray@(Ray origin direction) half_b a discriminant tmin tmax = r
    where
        root = sqrt discriminant
        mkRecord t =  HitRecord point normal material t front_face
            where
                point = pointAt ray t
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

instance Hittable Sphere where
    {-# INLINE hit #-}
    hit (Sphere center radius material) ray@(Ray origin direction) tmin tmax = record
        where
            (oc, a, half_b, c, discriminant) = {-# SCC "manual/hit_one" #-} hit_one center radius origin direction
            record = {-# SCC "manual/record" #-} if discriminant > 0 then hit_two center radius material ray half_b a discriminant tmin tmax else Nothing
         --   oc = origin - center
         --   a = lenSquared direction
         --   half_b = dot oc direction
         --   c = lenSquared oc - radius * radius
         --   discriminant = half_b * half_b - a * c
         --   {-# INLINE recordFn #-}
         --   recordFn root = r
         --       where
         --           mkRecord t' =  HitRecord point normal material t' front_face
         --               where
         --                   point = pointAt ray t'
         --                   outward_normal = (point - center) ./ radius
         --                   front_face = dot direction outward_normal < 0
         --                   normal = if front_face then outward_normal else -outward_normal
         --           t1 = (-half_b - root)/a
         --           t2 = (-half_b + root)/a
         --           r = if t1 < tmax && t1 > tmin 
         --                  then Just (mkRecord t1)
         --                  else if t2 < tmax && t2 > tmin
         --                       then Just (mkRecord t2)
         --                       else Nothing   
         --   record =  if discriminant > 0 then recordFn $ sqrt discriminant else Nothing

newtype HittableList a = HittableList (Array Int a)

toHittableList objects = HittableList $ listArray (1, length objects) objects 


printHistableList :: HittableList Sphere -> IO ()
printHistableList (HittableList arr) =  do
    let mats = fmap material (elems arr)
    let freqs = foldr (\(Material m) a -> matType m a) (0, 0, 0) mats
    print freqs

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
            reduce (r, current_max) i = fromMaybe (r, current_max) m
                where
                    m = do
                        record <- hit (unsafeAt items i) ray tmin current_max
                        return (Just record, t record)


reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v - (n .* (2 * (dot v n)))

data Material = forall a. Scatterable a => Material a

class Scatterable a where
    scatter :: RandomGen g => a -> Ray -> HitRecord -> g -> (Maybe (Ray, ColorVec), g)
    matType :: a -> (Int, Int, Int) -> (Int, Int, Int) 

data Lambertian = Lambertian ColorVec
data Metal = Metal ColorVec Double
data Dielectric = Dielectric Double

instance Scatterable Lambertian where
    matType (Lambertian _) (x, y, z) =  (x + 1, y, z)
    scatter (Lambertian color) _ray (HitRecord p normal _ _ _) g = (Just (scattered, color), g1)
        where
            (sampled, g1) = sampleUnitVector g
            scatter_direction = normal + sampled
            scattered = Ray p scatter_direction

instance Scatterable Metal where
    matType (Metal _ _) (x, y, z) =  (x, y + 1, z)
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
    matType (Dielectric _) (x, y, z) =  (x, y, z + 1)
    scatter (Dielectric refIdx) (Ray _ direction) rec g = (Just (scattered, attenuation), g1)
        where
            attenuation = one
            etai_over_etat = if front_face rec then 1.0 / refIdx else refIdx
            cos_theta = min (dot (negate $ unit direction) (normal rec)) 1.0
            sin_theta = sqrt (1.0 - cos_theta * cos_theta)
            reflect_probability = schlick cos_theta etai_over_etat
            -- (sampled, g1) = sampleFraction g
            -- scatter_direction = if etai_over_etat * sin_theta > 1.0 || reflect_probability > sampled
            g1 = g 
            scatter_direction = if etai_over_etat * sin_theta > 1.0 
                                   then reflect (unit direction) (normal rec)
                                   else refract (unit direction) (normal rec) etai_over_etat
            scattered = Ray (p rec) scatter_direction
