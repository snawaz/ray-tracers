

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns    #-}

module Image(
    writeImage
) where

import           Data.List             (foldl')
import           Data.Maybe            (fromMaybe)
import           Numeric.Limits        (maxValue)
import           System.Console.Pretty (pattern Green)
import qualified System.Console.Pretty as Pretty
import           System.Directory      (createDirectoryIfMissing, renameFile)
import           System.IO             (IOMode (WriteMode), hPutStrLn, withFile)
import           System.Random         (RandomGen, mkStdGen)
import           Text.Printf           (printf)
import           Control.DeepSeq       (force)
import           Control.Parallel.Strategies (using, rseq, parListChunk)

import           Camera                (camera, rayAt)
import           Colors                (Color, ColorVec, SampledColor (SampledColor), toColor)
import           Hittable              (HitRecord (HitRecord), Hittable (hit), Material (Material), scatter)
import           Ray                   (Ray (Ray))
import           Samplings             (sampleFraction)
import           Utils                 (getSecondsNow)
import           Vec                   (one, unit, vec, yCoor, zero, (.*))

data Image = Image {
        imageWidth  :: Int,
        imageHeight :: Int,
        imageColors :: [Color]
    } deriving (Show)

aspectRatio :: Double
aspectRatio = 16.0 / 9.0

writeImage :: Hittable a => Int -> Int -> Int -> a -> IO String
writeImage width samplesPerPixel raysPerSample world = do
    createDirectoryIfMissing True "images"
    let height = floor $ fromIntegral width / aspectRatio
    let image = createImage width height samplesPerPixel raysPerSample world
    start <- getSecondsNow
    let mkFilename secs rate = printf "images/%d.%dx%d.%d-%d.%dm-%ds.%d.ppm" rate width height samplesPerPixel raysPerSample (secs `div` 60) (secs `rem` 60) ((floor start) :: Int)
    withFile "images/tmp.ppm" WriteMode $ \h -> do
        hPutStrLn h "P3"
        hPutStrLn h $ show width ++ " " ++ show height
        hPutStrLn h $ show 255
        hPutStrLn h $ unlines $ fmap show $ imageColors image
    end <- getSecondsNow
    let elapsed = end - start
    let pixelRate =  fromIntegral (height * width) / elapsed
    let filename = mkFilename (floor elapsed :: Int) (floor pixelRate :: Int)
    renameFile "images/tmp.ppm" filename
    putStrLn $ "time elapsed      : " ++ Pretty.color Green (printf "%.3f" elapsed ++ " seconds")
    putStrLn $ "pixels per second : " ++ Pretty.color Green (printf "%.3f" pixelRate)
    putStrLn $ "Image produced    : " ++ Pretty.color Green filename
    return filename

createImage :: Hittable a => Int -> Int -> Int -> Int -> a -> Image
createImage width height samplesPerPixel raysPerSample world = Image width height (force colors)
    where
        cam = camera lookFrom lookAt viewUp 20.0 aspectRatio aperture focusDistance
            where
                lookFrom = vec 13 2 3
                lookAt = vec 0 0 0
                viewUp = vec 0 1 0
                focusDistance = 10.0
                aperture = 0.1
        coordinates = (,) <$> reverse [0..height-1] <*> [0..width-1]
        colors = fmap computeColor coordinates `using` parListChunk 32 rseq
        computeColor (j, i) = color
            where
                 g = mkStdGen (i * width + j)
                 sampledColor = fst $ foldl' sampledRayColor (SampledColor(samplesPerPixel, zero), g) [1..samplesPerPixel]
                 color = toColor sampledColor
                 sampledRayColor (acc, g'') _ = (force $ c + acc, g4)
                     where
                         (r1, g1) = sampleFraction g''
                         (r2, g2) = sampleFraction g1
                         u = (fromIntegral i + r1) / fromIntegral (width - 1)
                         v = (fromIntegral j + r2) / fromIntegral (height - 1)
                         (ray, g3) = rayAt cam u v g2
                         (colorVec, g4) = rayColor ray world g3 raysPerSample one
                         c =  SampledColor(samplesPerPixel, colorVec)

rayColor :: (Hittable a, RandomGen g) => Ray -> a -> g -> Int -> ColorVec -> (ColorVec, g)
rayColor ray@(Ray _origin direction) world g raysPerSample !acc =if raysPerSample <= 0 then (force zero, g) else computeColor
    where
        h = hit world ray 0.001 maxValue
        t = 0.5 * (yCoor (unit direction) + 1.0)
        default_color = one .* (1.0 -t) + (vec 0.5 0.7 1.0) .* t
        computeColor = fromMaybe (force $ acc * default_color, g) $ do
                                                record@(HitRecord _ _ (Material m) _ _ ) <- h
                                                let (maybeScattered, g1) = scatter m ray record g
                                                return $ fromMaybe (force zero, g1) $ do
                                                                (scatteredRay, attenuation) <- maybeScattered
                                                                return $ rayColor scatteredRay world g1 (raysPerSample - 1) (force $ acc * attenuation)

