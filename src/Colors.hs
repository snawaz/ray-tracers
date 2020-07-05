
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Colors where

import           BaseVec

type Color = BaseVec Int

type ColorVec = BaseVec Double

newtype SampledColor = SampledColor (Int, BaseVec Double)

instance Num SampledColor where
    (SampledColor(m, v1)) + (SampledColor (n, v2)) = SampledColor(n, v1 + v2)
    (SampledColor(m, v1)) * (SampledColor (n, v2)) = SampledColor(n, v1 * v2)
    (SampledColor(m, v1)) - (SampledColor (n, v2)) = SampledColor(n, v1 - v2)
    abs (SampledColor(n, v)) = SampledColor(n, abs v)
    signum (SampledColor(n, v)) = SampledColor(n, signum v)
    fromInteger n = SampledColor(fromInteger n, fromInteger 0)

class ToColor a where
    toColor :: a -> Color

clamp :: Ord a => a -> a -> a -> a
clamp lo hi val = min hi (max lo val)

instance ToColor SampledColor where
    toColor (SampledColor(n, v)) = fmap (floor . (256.0*) . clamp 0.0 0.999 . sqrt) $ v ./ fromIntegral n

toSampledColor :: Int -> ColorVec -> SampledColor
toSampledColor n color = SampledColor(n, color)

