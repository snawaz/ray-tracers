
{-# LANGUAGE NamedFieldPuns #-}

module Camera(
    Camera(Camera),
    rayAt,
    camera
 ) where

import           Ray (Ray (Ray))
import           Vec (Point3, Vec3, vec, (.*), (./))

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

rayAt :: Camera -> Double -> Double -> Ray
rayAt Camera{origin, lowerLeftCorner, horizontal, vertical} u v =
    Ray origin (lowerLeftCorner + horizontal .* u + vertical .* v - origin)
