
{-# LANGUAGE NamedFieldPuns #-}

module Image where

import           Data.List     (intercalate, foldl')
import           Data.Maybe
import           System.IO     (IOMode (WriteMode), hFlush, hPutStrLn, withFile)
import           System.Random
import           Text.Printf   (printf)
import Numeric.Limits(maxValue)

import           BaseVec
import           Camera
import           Colors
import           Hittable
import           Ray
import           Utils
import           Vec
import           Samplings

data Image = Image {
        width  :: Int,
        height :: Int,
        pixels :: [Color]
    } deriving (Show)

createImage :: (Hittable a) => Int -> Int -> Int -> a -> Image
createImage width height samplePerPixels world = Image width height pixels
    where
        lookFrom = vec 3 3 2
        lookAt = vec 0 0 (-1)
        viewUp = vec 0 1 0
        focusDistance = len $ lookFrom - lookAt
        aperture = 2.0
        cam = camera lookFrom lookAt viewUp 20.0 aspectRatio aperture focusDistance
        coordinates = (,) <$> [0..height-1] <*> reverse [0..width-1] -- no need to reverse y axis, as it'll be reversed by fold
        (pixels, _) = foldl' computeColor ([], mkStdGen 22) coordinates
        computeColor (colors, g) (j, i) = (color:colors, g')
            where
                (sampleColors, g') = foldl' sampleRayColor ([], g) [1..samplePerPixels]
                color = toColor $ foldl' (+) (SampledColor(samplePerPixels, vec 0 0 0)) sampleColors
                sampleRayColor (colors, g) _ = (c:colors, g4)
                    where
                        (r1, g1) = sampleFraction g
                        (r2, g2) = sampleFraction g1
                        u = (fromIntegral i + r1) / fromIntegral (width - 1)
                        v = (fromIntegral j + r2) / fromIntegral (height - 1)
                        (ray, g3) = getRay cam u v g2
                        (color, g4) = rayColor ray world g3 50
                        c = toSampledColor samplePerPixels color

writeImage :: (Hittable a) => Int -> Int -> a -> IO ()
writeImage imageWidth samplePerPixels world = do
    let image = createImage imageWidth (floor (fromIntegral imageWidth / aspectRatio)) samplePerPixels world
    let fmtColor (BaseVec [r,g,b]) = printf "%3d %3d %3d\n" r g b

    putStrLn "P3"
    putStrLn $ show (width image) ++ " " ++ show (height image)
    print 255
    putStrLn $ intercalate "\n" $ fmap fmtColor (pixels image)

rayColor :: (Hittable a, RandomGen g) => Ray -> a -> g -> Int -> (ColorVec, g)
rayColor ray@(Ray origin direction) world g depth = if depth <= 0 then (zero, g) else computeColor
    where
        h = hit world ray 0.001 maxValue
        unit_direction = unit direction
        t = 0.5 * (y (unit direction) + 1.0)
        default_color = one .* (1.0 - t) + vec 0.5 0.7 1.0 .* t
        computeColor = fromMaybe (default_color, g) $ do
                                                rec@(HitRecord p normal (Material m) _ _ ) <- h
                                                let (maybeScattered, g1) = scatter m ray rec g
                                                return $ fromMaybe (zero, g1) $ do
                                                                (scattered, attenuation) <- maybeScattered
                                                                let (c, g2) = rayColor scattered world g1 (depth - 1)
                                                                return (attenuation * c, g2)

hitSphere center radius (Ray origin direction) = if discriminant < 0
                                                    then -1.0
                                                    else (-half_b - sqrt discriminant) / a
    where
        oc = origin - center
        a = len2 direction
        half_b = dot oc direction
        c = len2 oc - radius^2
        discriminant = half_b^2 - a*c
