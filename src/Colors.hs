
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Colors where

import           BaseVec

type Color = BaseVec Int

-- type FractionalColor = BaseVec Double

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

-- instance ToColor FractionalColor where
--    toColor = fmap (floor . (255.99 *))

instance ToColor SampledColor where
    -- toColor (SampledColor(n, v)) = fmap floor $ v ./ fromIntegral n
    toColor (SampledColor(n, v)) = fmap (floor . (255.99*) . sqrt) $ v ./ fromIntegral n

toSampledColor :: Int -> ColorVec -> SampledColor
toSampledColor n color = SampledColor(n, color)

