
{-# LANGUAGE NamedFieldPuns #-}

module Camera(
    Camera(Camera),
    rayAt,
    camera
 ) where

import           Ray (Ray (Ray))
import           Vec (Point3, Vec3, cross, unit, (.*), (./))

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

rayAt :: Camera -> Double -> Double -> Ray
rayAt Camera{origin, lowerLeftCorner, horizontal, vertical} u v =
    Ray origin (lowerLeftCorner + horizontal .* u + vertical .* v - origin)
