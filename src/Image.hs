
module Image(
    writeImage
) where

import           Data.List      (intercalate)
import           Data.Maybe     (fromMaybe)
import           Numeric.Limits (maxValue)

import           Colors         (Color, toColor)
import           Hittable       (HitRecord (HitRecord), Hittable (hit), HittableList (HittableList), Sphere (Sphere))
import           Ray            (Ray (Ray))
import           Vec            (unit, vec, yCoor, (.*), (.+), (./))

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
        viewportHeight = 2.0
        viewportWidth = aspectRatio * viewportHeight
        focalLength = 1.0
        origin = vec 0 0 0
        horizontal = vec viewportWidth 0 0
        vertical = vec 0 viewportHeight 0
        loweLeftCorner = origin - horizontal ./ 2 - vertical ./ 2 - vec 0 0 focalLength
        colors = do
                j <- reverse [0..height-1]
                return $ do
                    i <- [0..width-1]
                    let u = fromIntegral i / fromIntegral (width - 1)
                    let v = fromIntegral j / fromIntegral (height - 1)
                    let r = Ray origin (loweLeftCorner +  horizontal .* u + vertical .* v - origin)
                    return $ rayColor r world

writeImage :: IO ()
writeImage = do
    let image = createImage 384 $ floor (384 / aspectRatio)
    putStrLn "P3"
    putStrLn $ show (imageWidth image) ++ " " ++ show (imageHeight image)
    print 255
    putStrLn $ unlines [ intercalate "    " [show c | c <- row] | row <- imageColors image]

rayColor :: Hittable a => Ray -> HittableList a -> Color
rayColor (Ray origin direction) world = toColor $ fromMaybe default_color $ do
                                                     (HitRecord _ normal _ _) <- h
                                                     return $ (normal .+ 1) .* 0.5
    where
        h = hit world (Ray origin direction) 0 maxValue
        t = 0.5 * (yCoor (unit direction) + 1.0)
        default_color = (vec 1 1 1) .* (1.0 -t) + (vec 0.5 0.7 1.0) .* t
