
module Ray where

import           BaseVec
import           Vec

data Ray = Ray Point3 Vec3

pointAt (Ray origin direction) t = origin + direction .* t
