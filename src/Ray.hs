
module Ray(
    Ray(Ray),
    origin, direction, pointAt
 )where

import           Vec (Point3, Vec3, (.*))

data Ray = Ray {
    origin    :: !Point3,
    direction :: !Vec3
}

pointAt :: Ray -> Double -> Point3
pointAt (Ray origin' direction') t = origin' + direction' .* t
