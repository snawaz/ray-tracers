
module Colors(
    Color,
    toColor
) where

import           Text.Printf (printf)

newtype Color = Color (Int, Int, Int)

instance Show Color where
    show (Color(r,g,b)) = printf "%3d %3d %3d" r g b

toColor :: Int -> Int -> Int ->Int -> Color
toColor i j w h = Color(r, g, b)
    where
        r = floor $ 255.99 * fromIntegral i / fromIntegral (w - 1)
        g = floor $ 255.99 * fromIntegral j / fromIntegral (h - 1)
        b = floor $ 255.99 * 0.25
