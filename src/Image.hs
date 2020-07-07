
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

module Image where

import           Data.List     (intercalate, foldl')
import           Data.Maybe
import           System.IO     (IOMode (WriteMode), hFlush, hPutStrLn, withFile)
import           System.Random
import           Text.Printf   (printf)
import Numeric.Limits(maxValue)
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Parallel
import Control.Parallel.Strategies (using, rpar, rseq, rdeepseq, parListChunk)
import           Data.Time.Clock.System
import           System.IO                  (IOMode (WriteMode),  withFile, hPutStrLn)
import           System.Directory (renameFile)

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

createImage :: (Hittable a) => Int -> Int -> Int -> a -> Int -> Image
createImage width height samplePerPixels world depth = Image width height (pixels)
    where
        lookFrom = vec 13 2 3
        lookAt = vec 0 0 0
        viewUp = vec 0 1 0
        focusDistance = 10.0 -- len $ lookFrom - lookAt
        aperture = 0.1 --2.0
        cam = camera lookFrom lookAt viewUp 20.0 aspectRatio aperture focusDistance
        coordinates = (,) <$> reverse [0..height-1] <*> [0..width-1]
        pixels = force map computeColor coordinates `using` parListChunk 32 rseq
        computeColor (j, i) = color
            where
                g = mkStdGen (i * width + j)
                (sampleColor, g') = foldl' sampleRayColor (SampledColor(samplePerPixels, zero), g) [1..samplePerPixels]
                color = force $ toColor sampleColor
                sampleRayColor (acc, g) _ = (force $ c + acc, g4)
                    where
                        (r1, g1) = sampleFraction g
                        (r2, g2) = sampleFraction g1
                        u = (fromIntegral i + r1) / fromIntegral (width - 1)
                        v = (fromIntegral j + r2) / fromIntegral (height - 1)
                        (ray, g3) = getRay cam u v g2
                        (color, g4) = rayColor ray world g3 depth one
                        c = toSampledColor samplePerPixels color

rayColor :: (Hittable a, RandomGen g) => Ray -> a -> g -> Int -> ColorVec -> (ColorVec, g)
rayColor ray@(Ray origin direction) world g depth !acc = if depth <= 0 then (force zero, g) else computeColor
--rayColor ray@(Ray origin direction) world g depth = if depth <= 0 then (zero, g) else computeColor
    where
        h = hit world ray 0.001 maxValue
        unit_direction = unit direction
        t = 0.5 * (y (unit direction) + 1.0)
        default_color = one .* (1.0 - t) + vec 0.5 0.7 1.0 .* t
        computeColor = fromMaybe (force $ default_color * acc, g) $ do
        --computeColor = fromMaybe (default_color, g) $ do
                                                rec@(HitRecord p normal (Material m) _ _ ) <- h
                                                let (maybeScattered, g1) = scatter m ray rec g
                                                return $ fromMaybe (force zero, g1) $ do
                                                --return $ fromMaybe (zero, g1) $ do
                                                                (scattered, attenuation) <- maybeScattered
                                                                return $ rayColor scattered world g1 (depth - 1) (acc * attenuation)
                                                                -- let (c, g2) = rayColor scattered world g1 (depth - 1)
                                                                -- return (force $ attenuation * c, g2)
                                                                -- return (attenuation * c, g2)

writeImage :: (Hittable a) => Int -> Int -> a -> Int -> IO ()
writeImage imageWidth samplePerPixels world depth = do
    let imageHeight = floor $ fromIntegral imageWidth / aspectRatio
    let image = createImage imageWidth imageHeight samplePerPixels world depth
    let fmtColor (BaseVec r g b) = printf "%3d %3d %3d" r g b
    now <- getSecondsNow
    let filename seconds = printf "images/%dx%d-%d-%d-%dm-%ds-%d.ppm" imageWidth imageHeight samplePerPixels depth (seconds `div` 60) (seconds `mod` 60) now
    -- colors <- evaluate $ force $ (pixels image) `using` parListChunk 32 rseq
    -- let colors = pixels image

    (colors, elapsed1) <- getSecondsElapsed $ evaluate $ force $ pixels image

    (_, elapsed2) <- getSecondsElapsed $ withFile "images/tmp.ppm" WriteMode $ \h -> do
        hPutStrLn h "P3"
        hPutStrLn h $ show (width image) ++ " " ++ show (height image)
        hPutStrLn h $ show 255
        hPutStrLn h $ intercalate "\t" $ fmap fmtColor colors
    renameFile "images/tmp.ppm" (filename (elapsed1 + elapsed2))
    putStrLn $ "\n colors: " ++ show elapsed1 ++ ", writing file: " ++  show elapsed2

hitSphere center radius (Ray origin direction) = if discriminant < 0
                                                    then -1.0
                                                    else (-half_b - sqrt discriminant) / a
    where
        oc = origin - center
        a = len2 direction
        half_b = dot oc direction
        c = len2 oc - radius^2
        discriminant = half_b^2 - a*c
