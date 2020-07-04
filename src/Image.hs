
{-# LANGUAGE NamedFieldPuns #-}

module Image where

import           Data.List     (intercalate, foldl')
import           Data.Maybe
import           System.IO     (IOMode (WriteMode), hFlush, hPutStrLn, withFile)
import           System.Random
import           Text.Printf   (printf)

import           BaseVec
import           Camera
import           Colors
import           Hittable
import           Ray
import           Utils
import           Vec

data Image = Image {
        width  :: Int,
        height :: Int,
        pixels :: [[Color]]
    } deriving (Show)

randomFraction :: (RandomGen g) => g -> (Double, g)
randomFraction g = randomR (0, 1) g

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

createImage' :: (Hittable a) => Int -> Int -> Int -> [Double] -> a -> Image
createImage' width height samplePerPixels randList world = Image width height (chunksOf width pixels)
    where
        coordinates = (,) <$> reverse [0..height-1] <*> [0..width-1]
        (pixels, _) = foldl' computeColor ([], mkStdGen 22) coordinates
        computeColor (colors, g) (j, i) = (color:colors, g')
            where
                (sampleColors, g') = foldl' randomRayColor ([], g) [1..samplePerPixels]
                color = toColor $ foldl' (+) (SampledColor(samplePerPixels, vec 0 0 0)) sampleColors
                randomRayColor (colors, g) _ = (c:colors, g2)
                    where
                        (r1, g1) = randomFraction g
                        (r2, g2) = randomFraction g1
                        u = (fromIntegral i + r1) / fromIntegral (width - 1)
                        v = (fromIntegral j + r2) / fromIntegral (height - 1)
                        c = toSampledColor samplePerPixels $ rayColor (ray camera u v) world

createImage :: (Hittable a) => Int -> Int -> Int -> [Double] -> a -> Image
createImage width height samplePerPixels randList world = Image width height pixels
    where
        pixels = do
            j <- reverse [0..height - 1]
            return $ do
                i <- [0..width - 1]
                let rands = take samplePerPixels randList
                let sampledColor = foldr (+) (SampledColor(samplePerPixels, vec 0 0 0)) $ fmap (randomRayColor i j world) rands
                return $ toColor sampledColor
        randomRayColor i j world rand = toSampledColor samplePerPixels $ rayColor (ray camera u v) world
            where
                u = (fromIntegral i + rand) / fromIntegral (width - 1)
                v = (fromIntegral j + rand) / fromIntegral (height - 1)

writeImage :: (Hittable a) => Int -> Int -> a -> IO ()
writeImage imageWidth samplePerPixels world = do
    let gen = mkStdGen 22
    let randList = (randomRs (0,1) gen) :: [Double]
    let image = createImage imageWidth (floor (fromIntegral imageWidth / aspectRatio)) samplePerPixels randList world
    let fmtColor (BaseVec [r,g,b]) = printf "%3d %3d %3d\n" r g b

    putStrLn "P3"
    putStrLn $ show (width image) ++ " " ++ show (height image)
    print 255
    putStrLn $ unlines [ intercalate "    " [fmtColor c | c <- row] | row <- pixels image]

rayColor :: (Hittable a) => Ray -> a -> Color
rayColor ray@(Ray origin direction) world = toColor $ fromMaybe default_color $ do
                                                (HitRecord _ normal _ _ ) <- h
                                                return $ (normal .+ 1) .* 0.5
    where
        h = hit world ray 0 100000000000
        unit_direction = unit direction
        t = 0.5 * (y (unit direction) + 1.0)
        default_color = vec 1 1 1 .* (1.0 - t) + vec 0.5 0.7 1.0 .* t

hitSphere center radius (Ray origin direction) = if discriminant < 0
                                                    then -1.0
                                                    else (-half_b - sqrt discriminant) / a
    where
        oc = origin - center
        a = len2 direction
        half_b = dot oc direction
        c = len2 oc - radius^2
        discriminant = half_b^2 - a*c
