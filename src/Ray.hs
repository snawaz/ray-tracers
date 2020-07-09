
module Ray(
    Ray(Ray),
    pointAt
 )where

import           Vec (Point3, Vec3, (.*))

data Ray = Ray Point3 Vec3

pointAt :: Ray -> Double -> Point3
pointAt (Ray origin direction) t = origin + direction .* t
