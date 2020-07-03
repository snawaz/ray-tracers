
module Ray where

import           Vec
import           BaseVec

data Ray = Ray Point3 Vec3

pointAt (Ray origin direction) t = origin + direction .* t
