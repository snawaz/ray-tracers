
module Image(
    writeImage
) where

import           Data.List      (foldl')
import           Data.Maybe     (fromMaybe)
import           Numeric.Limits (maxValue)
import           System.Random  (mkStdGen, RandomGen, randomR)

import           Camera         (camera, rayAt)
import           Colors         (Color, ColorVec, SampledColor (SampledColor), toColor)
import           Hittable       (HitRecord (HitRecord), Hittable (hit), HittableList (HittableList), Sphere (Sphere))
import           Ray            (Ray (Ray))
import           Vec            (unit, vec, yCoor, zero, (.*), (.+))

data Image = Image {
        imageWidth  :: Int,
        imageHeight :: Int,
        imageColors :: [Color]
    } deriving (Show)

aspectRatio :: Double
aspectRatio = 16.0 / 9.0;

sampledFracton :: (RandomGen g) => g -> (Double, g)
sampledFracton g = randomR (0, 1) g

createImage :: Int -> Int -> Image
createImage width height = Image width height colors
    where
        world = HittableList [
                (Sphere (vec 0 0 (-1)) 0.5),
                (Sphere (vec 0 (-100.5) (-1)) 100)
            ]
        samplePerPixels = 100
        coordinates = (,) <$> [0..height-1] <*> reverse [0..width-1]
        colors = fst $ foldl' computeColor ([], mkStdGen 22) coordinates
        computeColor (colors', g) (j, i) = (color:colors', g')
             where
                 (sampledColor, g') = foldl' sampledRayColor (SampledColor(samplePerPixels, zero), g) [1..samplePerPixels]
                 color = toColor sampledColor
                 sampledRayColor (acc, g'') _ = (c + acc, g2)
                     where
                         (r1, g1) = sampledFracton g''
                         (r2, g2) = sampledFracton g1
                         u = (fromIntegral i + r1) / fromIntegral (width - 1)
                         v = (fromIntegral j + r2) / fromIntegral (height - 1)
                         c =  SampledColor(samplePerPixels, rayColor (rayAt camera u v) world)

writeImage :: IO ()
writeImage = do
    let width = 100 -- 384
    let image = createImage width $ floor (fromIntegral width / aspectRatio)
    putStrLn "P3"
    putStrLn $ show (imageWidth image) ++ " " ++ show (imageHeight image)
    print 255
    putStrLn $ unlines $ fmap show $ imageColors image

rayColor :: Hittable a => Ray -> a -> ColorVec
rayColor ray@(Ray _origin direction) world = fromMaybe default_color $ do
                                                    (HitRecord _ normal _ _) <- h
                                                    return $ (normal .+ 1) .* 0.5
    where
        h = hit world ray 0 maxValue
        t = 0.5 * (yCoor (unit direction) + 1.0)
        default_color = (vec 1 1 1) .* (1.0 -t) + (vec 0.5 0.7 1.0) .* t
