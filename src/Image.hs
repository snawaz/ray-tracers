
module Image(
    writeImage
) where

import           Data.List      (foldl', intercalate)
import           Data.Maybe     (fromMaybe)
import           Numeric.Limits (maxValue)
import           System.Random  (mkStdGen, randomRs)

import           Camera         (camera, rayAt)
import           Colors         (Color, ColorVec, SampledColor (SampledColor), toColor)
import           Hittable       (HitRecord (HitRecord), Hittable (hit), HittableList (HittableList), Sphere (Sphere))
import           Ray            (Ray (Ray))
import           Vec            (unit, vec, yCoor, zero, (.*), (.+))

data Image = Image {
        imageWidth  :: Int,
        imageHeight :: Int,
        imageColors :: [[Color]]
    } deriving (Show)

aspectRatio :: Double
aspectRatio = 16.0 / 9.0;

createImage :: Int -> Int -> Image
createImage width height = Image width height colors
    where
        world = HittableList [
                (Sphere (vec 0 0 (-1)) 0.5),
                (Sphere (vec 0 (-100.5) (-1)) 100)
            ]
        gen = mkStdGen 22
        randList = (randomRs (0,1) gen) :: [Double]
        samplePerPixels = 100
        colors = do
                j <- reverse [0..height-1]
                return $ do
                    i <- [0..width-1]
                    let rands = take samplePerPixels randList
                    let colorVec = foldl' (+) zero $ fmap (randomRayColor i j world) rands
                    return . toColor $ SampledColor(samplePerPixels, colorVec)
        randomRayColor i j world' rand = rayColor (rayAt camera u v) world'
                where
                    u = (fromIntegral i + rand) / fromIntegral (width - 1)
                    v = (fromIntegral j + rand) / fromIntegral (height - 1)

writeImage :: IO ()
writeImage = do
    let image = createImage 384 $ floor (384 / aspectRatio)
    putStrLn "P3"
    putStrLn $ show (imageWidth image) ++ " " ++ show (imageHeight image)
    print 255
    putStrLn $ unlines [ intercalate "    " [show c | c <- row] | row <- imageColors image]

rayColor :: Hittable a => Ray -> HittableList a -> ColorVec
rayColor (Ray origin direction) world = fromMaybe default_color $ do
                                                    (HitRecord _ normal _ _) <- h
                                                    return $ (normal .+ 1) .* 0.5
    where
        h = hit world (Ray origin direction) 0 maxValue
        t = 0.5 * (yCoor (unit direction) + 1.0)
        default_color = (vec 1 1 1) .* (1.0 -t) + (vec 0.5 0.7 1.0) .* t
