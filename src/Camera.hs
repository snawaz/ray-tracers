
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS -Wno-unused-matches #-}

module Camera(
    Camera(Camera),
    rayAt,
    camera
 ) where

import           System.Random (RandomGen)

import           Ray           (Ray (Ray))
import           Samplings     (sampleVecInUnitDisk)
import           Vec           (Point3, Vec3, cross, unit, (.*), (./), xCoor, yCoor)


data Camera = Camera {
    origin          :: Point3,
    lowerLeftCorner :: Point3,
    horizontal      :: Vec3,
    vertical        :: Vec3,
    u, v, w         :: Vec3,
    lensRadius      :: Double
}

camera :: Point3 -> Point3 -> Vec3 -> Double -> Double -> Double -> Double -> Camera
camera lookFrom lookAt viewUp verticalFov aspectRatio aperture focusDistance = Camera { origin, lowerLeftCorner, horizontal, vertical, u, v, w, lensRadius }
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

rayAt :: RandomGen g => Camera -> Double -> Double -> g -> (Ray, g)
rayAt Camera{origin, lowerLeftCorner, horizontal, vertical, u, v, w, lensRadius} s t g = (r, g1)
    where
        (rd', g1) = sampleVecInUnitDisk g
        rd = rd' .* lensRadius
        offset = u .* (xCoor rd) + v .* (yCoor rd)
        orig = origin + offset
        r = Ray orig (lowerLeftCorner + horizontal .* s + vertical .* t - orig)
