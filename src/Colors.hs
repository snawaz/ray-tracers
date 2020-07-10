
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Colors(
    Color,
    ColorVec,
    SampledColor(SampledColor),
    toColor
) where

import           Text.Printf (printf)
import           Control.DeepSeq       (NFData, rnf)

import           Vec         (Vec (Vec), (./))

newtype Color = Color (Int, Int, Int)

type ColorVec = Vec Double

newtype SampledColor = SampledColor (Int, ColorVec)

instance Show Color where
    show (Color(r,g,b)) = printf "%3d %3d %3d" r g b

class ToColor a where
    toColor :: a -> Color

instance NFData Color where
    rnf (Color(r, g, b)) = rnf r `seq` rnf g `seq` rnf b

instance Num SampledColor where
    (SampledColor(_, v1)) + (SampledColor (n, v2)) = SampledColor(n, v1 + v2)
    (SampledColor(_, v1)) * (SampledColor (n, v2)) = SampledColor(n, v1 * v2)
    (SampledColor(_, v1)) - (SampledColor (n, v2)) = SampledColor(n, v1 - v2)
    abs (SampledColor(n, v)) = SampledColor(n, abs v)
    signum (SampledColor(n, v)) = SampledColor(n, signum v)
    fromInteger n = SampledColor(fromInteger n, fromInteger 0)

instance NFData SampledColor where
    rnf (SampledColor(n, v)) = rnf n `seq` rnf v

clamp :: Ord a => a -> a -> a -> a
clamp lo hi val = min hi (max lo val)

instance ToColor SampledColor where
    toColor (SampledColor(n, v)) = Color(xs !! 0, xs !! 1, xs !! 2)
         where
             (Vec xs) = fmap (floor . (256.0*) . clamp 0.0 0.999 . sqrt) $ v ./ fromIntegral n

