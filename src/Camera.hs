
{-# LANGUAGE NamedFieldPuns #-}

module Camera where

import           BaseVec
import           Ray
import           Vec

aspectRatio = 16.0 / 9.0;
-- viewportHeight = 2.0
-- viewportWidth = aspectRatio * viewportHeight
-- focalLength = 1.0

data Camera = Camera {
    origin          :: Point3,
    lowerLeftCorner :: Point3,
    horizontal      :: Vec3,
    vertical        :: Vec3
}

camera :: Point3 -> Point3 -> Vec3 -> Double -> Double -> Camera
camera lookFrom lookAt viewUp verticalFov aspectRatio = Camera { origin, lowerLeftCorner, horizontal, vertical }
    where
        theta = verticalFov * pi / 180.0
        h = tan (theta / 2.0)
        viewportHeight = 2.0 * h
        viewportWidth = aspectRatio * viewportHeight

        w = unit $ lookFrom - lookAt
        u = unit $ cross viewUp w
        v = cross w u

        origin = lookFrom
        horizontal = u .* viewportWidth
        vertical = v .* viewportHeight
        lowerLeftCorner = origin - horizontal ./ 2 - vertical ./ 2 - w

ray Camera{origin, lowerLeftCorner, horizontal, vertical} u v =
    Ray origin (lowerLeftCorner + horizontal .* u + vertical .* v - origin)
