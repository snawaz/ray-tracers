{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Colors(
    Color,
    ColorVec,
    SampledColor(SampledColor),
    toColor
) where

import           Control.DeepSeq (NFData, rnf)
import           Control.DeepSeq (force)

import           Vec             (Vec (Vec), (./))

newtype AnyColor a = AnyColor (Vec a) deriving(Functor, Num)

type ColorVec = AnyColor Double

type Color = AnyColor Int

newtype SampledColor = SampledColor (Int, ColorVec)

instance Show a => Show (AnyColor a) where
    show (AnyColor (Vec r g b)) = show r ++ " " ++ show g ++ " " ++ show b

class ToColor a b | b -> a where
    toColor :: a -> AnyColor b

instance ToColor (Vec Double) Double where
    toColor v = AnyColor v

instance NFData a => NFData (AnyColor a) where
    rnf (AnyColor v) = rnf v 

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

instance ToColor SampledColor Int where
    toColor (SampledColor(n, AnyColor v)) = AnyColor $ force $ fmap (floor . (256.0*) . clamp 0.0 0.999 . sqrt) $ v ./ fromIntegral n
