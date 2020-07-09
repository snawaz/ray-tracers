
module Ray(
    Ray (Ray),
    pointAt
) where

import           BaseVec
import           Vec

data Ray = Ray {
    origin :: !Point3,
    direction :: !Vec3
}

pointAt (Ray origin direction) t = origin + direction .* t
