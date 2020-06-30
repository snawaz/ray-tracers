
module Ray where

import Vec

data Ray = Ray Point3 Vec3

-- origin (Ray point _) = point
--
-- direction (Ray _ vector) = vector

pointAt (Ray origin direction) t = origin + apply (*t) direction
