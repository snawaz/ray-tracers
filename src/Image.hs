
module Image(
    writeImage
) where

import           Data.List (intercalate)

import           Colors    (Color, toColor)
import           Ray       (Ray (Ray), pointAt)
import           Vec       (Point3, dot, lenSquared, unit, vec, yCoor, (.*), (.+), (./))

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
                    return $ rayColor r

writeImage :: IO ()
writeImage = do
    let image = createImage 384 $ floor (384 / aspectRatio)
    putStrLn "P3"
    putStrLn $ show (imageWidth image) ++ " " ++ show (imageHeight image)
    print 255
    -- putStrLn $ unlines [color c | row <- pixelColors image, c <- row]
    putStrLn $ unlines [ intercalate "    " [show c | c <- row] | row <- imageColors image]



rayColor :: Ray -> Color
rayColor (Ray origin direction) = toColor $ if h > 0.0
                                                then ((unit $ pointAt (Ray origin direction) h - vec 0 0 (-1)) .+ 1) .* 0.5
                                                else p1 + p2
    where
        h = hitSphere (vec 0 0 (-1)) 0.5 (Ray origin direction)
        t = 0.5 * (yCoor (unit direction) + 1.0)
        p1 = (vec 1 1 1) .* (1.0 -t)
        p2 = (vec 0.5 0.7 1.0) .* t


hitSphere :: Point3 -> Double -> Ray -> Double
hitSphere center radius (Ray origin direction) = if discriminant < 0
                                                    then -1.0
                                                    else (-half_b - sqrt discriminant) / a
    where
        oc = origin - center
        a = lenSquared direction
        half_b = dot oc direction
        c = lenSquared oc - radius^2
        discriminant = half_b^2 - 4*a*c
