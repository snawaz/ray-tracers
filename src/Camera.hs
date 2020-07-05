
{-# LANGUAGE NamedFieldPuns #-}

module Camera where

import System.Random

import           BaseVec
import           Ray
import           Vec
import           Samplings

aspectRatio = 16.0 / 9.0;
-- viewportHeight = 2.0
-- viewportWidth = aspectRatio * viewportHeight
-- focalLength = 1.0

data Camera = Camera {
    origin          :: Point3,
    lowerLeftCorner :: Point3,
    horizontal      :: Vec3,
    vertical        :: Vec3,
    u, v, w         :: Vec3,
    lensRadius      :: Double
}

camera :: Point3 -> Point3 -> Vec3 -> Double -> Double -> Double -> Double -> Camera
camera lookFrom lookAt viewUp verticalFov aspectRatio aperture focusDistance = Camera {origin, lowerLeftCorner, horizontal, vertical, u, v, w, lensRadius}
    where
        theta = verticalFov * pi / 180.0
        h = tan (theta / 2.0)
        viewportHeight = 2.0 * h
        viewportWidth = aspectRatio * viewportHeight

        w = unit $ lookFrom - lookAt
        u = unit $ cross viewUp w
        v = cross w u

        origin = lookFrom
        horizontal = u .* viewportWidth .* focusDistance
        vertical = v .* viewportHeight .* focusDistance
        lowerLeftCorner = origin - horizontal ./ 2 - vertical ./ 2 - w .* focusDistance
        lensRadius = aperture / 2.0

getRay :: RandomGen g => Camera -> Double -> Double -> g -> (Ray, g)
getRay Camera{origin, lowerLeftCorner, horizontal, vertical, u, v, w, lensRadius} s t g = (r, g1)
    where
        (rd', g1) = sampleVecInUnitDisk g
        rd = rd' .* lensRadius
        offset = u .* (x rd) + v .* (y rd)
        orig = origin + offset
        r = Ray orig (lowerLeftCorner + horizontal .* s + vertical .* t - orig)
