
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Colors(
    Color,
    toColor
) where

import           Text.Printf (printf)

import           Vec         (Point3, Vec (Vec))

newtype Color = Color (Int, Int, Int)

instance Show Color where
    show (Color(r,g,b)) = printf "%3d %3d %3d" r g b

class ToColor a where
    toColor :: a -> Color

instance ToColor Point3 where
    toColor (Vec xs) = Color (r, g, b)
        where
            r = floor $ 255.99 * xs !! 0
            g = floor $ 255.99 * xs !! 1
            b = floor $ 255.99 * xs !! 2
