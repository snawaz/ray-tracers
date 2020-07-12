
module Ray(
    Ray(Ray),
    getOrigin, getDirection, pointAt
 )where

import           Vec (Point3, Vec3, (.*))

data Ray = Ray {
    getOrigin    :: !Point3,
    getDirection :: !Vec3
}

pointAt :: Ray -> Double -> Point3
pointAt (Ray origin direction) t = origin + direction .* t
