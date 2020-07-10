
module Image(
    writeImage
) where

import           Data.List      (foldl')
import           Data.Maybe     (fromMaybe)
import           Numeric.Limits (maxValue)
import           System.Random  (RandomGen, mkStdGen)

import           Camera         (camera, rayAt)
import           Colors         (Color, ColorVec, SampledColor (SampledColor), toColor)
import           Hittable       ( HitRecord (HitRecord), Hittable (hit), HittableList (HittableList), Lambertian (Lambertian),
                                 Material (Material), Sphere (Sphere), scatter)
import           Ray            (Ray (Ray))
import           Samplings      (sampleFraction)
import           Vec            (one, unit, vec, yCoor, zero, (.*))

data Image = Image {
        imageWidth  :: Int,
        imageHeight :: Int,
        imageColors :: [Color]
    } deriving (Show)

aspectRatio :: Double
aspectRatio = 16.0 / 9.0

createImage :: Int -> Int -> Image
createImage width height = Image width height colors
    where
        r = cos (pi / 4)
        world = HittableList [
                (Sphere (vec (-r) 0 (-1)) r (Material (Lambertian (vec 0 0 1)))),
                (Sphere (vec r 0 (-1)) r (Material (Lambertian (vec 1 0 0))))
            ]
        samplePerPixels = 100
        depth = 50
        cam = camera 90.0 aspectRatio
        coordinates = (,) <$> [0..height-1] <*> reverse [0..width-1]
        colors = fst $ foldl' computeColor ([], mkStdGen 22) coordinates
        computeColor (colors', g) (j, i) = (color:colors', g')
             where
                 (sampledColor, g') = foldl' sampledRayColor (SampledColor(samplePerPixels, zero), g) [1..samplePerPixels]
                 color = toColor sampledColor
                 sampledRayColor (acc, g'') _ = (c + acc, g3)
                     where
                         (r1, g1) = sampleFraction g''
                         (r2, g2) = sampleFraction g1
                         u = (fromIntegral i + r1) / fromIntegral (width - 1)
                         v = (fromIntegral j + r2) / fromIntegral (height - 1)
                         (colorVec, g3) = rayColor (rayAt cam u v) world g2 depth
                         c =  SampledColor(samplePerPixels, colorVec)

writeImage :: IO ()
writeImage = do
    let width = 100 -- 384
    let image = createImage width $ floor (fromIntegral width / aspectRatio)
    putStrLn "P3"
    putStrLn $ show (imageWidth image) ++ " " ++ show (imageHeight image)
    print 255
    putStrLn $ unlines $ fmap show $ imageColors image

rayColor :: (Hittable a, RandomGen g) => Ray -> a -> g -> Int -> (ColorVec, g)
rayColor ray@(Ray _origin direction) world g depth =if depth <= 0 then (zero, g) else computeColor
    where
        h = hit world ray 0.001 maxValue
        t = 0.5 * (yCoor (unit direction) + 1.0)
        default_color = one .* (1.0 -t) + (vec 0.5 0.7 1.0) .* t
        computeColor = fromMaybe (default_color, g) $ do
                                                record@(HitRecord _ _ (Material m) _ _ ) <- h
                                                let (maybeScattered, g1) = scatter m ray record g
                                                return $ fromMaybe (zero, g1) $ do
                                                                (scatteredRay, attenuation) <- maybeScattered
                                                                let (c, g2) = rayColor scatteredRay world g1 (depth - 1)
                                                                return (attenuation * c, g2)

