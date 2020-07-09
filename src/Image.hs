
module Image(
    writeImage
) where

import           Data.List (intercalate)

import           Colors    (Color (Color))

data Image = Image {
        imageWidth  :: Int,
        imageHeight :: Int,
        imageColors :: [[Color]]
    } deriving (Show)

createImage :: Int -> Int -> Image
createImage width height = Image width height colors
    where
        colors = do
                j <- reverse [0..height-1]
                let makeColor i' j' = Color(r, g, b)
                        where
                            r = floor $ 255.99 * fromIntegral i' / fromIntegral (width - 1)
                            g = floor $ 255.99 * fromIntegral j' / fromIntegral (height - 1)
                            b = floor $ 255.99 * 0.25
                return [makeColor i j | i <- [0..width-1]]

writeImage :: IO ()
writeImage = do
    let image = createImage 256 256
    putStrLn "P3"
    putStrLn $ show (imageWidth image) ++ " " ++ show (imageHeight image)
    print 255
    -- putStrLn $ unlines [color c | row <- pixelColors image, c <- row]
    putStrLn $ unlines [ intercalate "    " [show c | c <- row] | row <- imageColors image]
