
module Colors(
    Color(Color)
) where

import           Text.Printf (printf)

newtype Color = Color (Int, Int, Int)

instance Show Color where
    show (Color(r,g,b)) = printf "%3d %3d %3d" r g b

