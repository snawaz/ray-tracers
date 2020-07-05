
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

camera :: Double -> Double -> Camera
camera verticalFov aspectRatio = Camera { origin, lowerLeftCorner, horizontal, vertical }
    where
        theta = verticalFov * pi / 180.0
        h = tan (theta / 2.0)
        viewportHeight = 2.0 * h
        viewportWidth = aspectRatio * viewportHeight
        focalLength = 1.0
        origin = vec 0 0 0
        horizontal = vec viewportWidth 0 0
        vertical = vec 0 viewportHeight 0
        lowerLeftCorner = origin - horizontal ./ 2 - vertical ./ 2 - vec 0 0 focalLength

ray Camera{origin, lowerLeftCorner, horizontal, vertical} u v =
    Ray origin (lowerLeftCorner + horizontal .* u + vertical .* v - origin)
