
{-# LANGUAGE DeriveFunctor #-}

module Vec(
    Vec (Vec),
    zero, one, from ,vec,
    xCoor, yCoor, zCoor,
    dot, cross, unit,
    (.+), (.-), (.*), (./),
    Point3, Vec3,
    len, lenSquared
)where

import           Control.DeepSeq (NFData, rnf)

data Vec a = Vec {
    xCoor :: a,
    yCoor :: a,
    zCoor :: a
} deriving(Show, Functor)

instance Num a => Num (Vec a) where
    Vec x1 y1 z1 + Vec x2 y2 z2 = Vec (x1 + x2) (y1 + y2) (z1 + z2)
    Vec x1 y1 z1 - Vec x2 y2 z2 = Vec (x1 - x2) (y1 - y2) (z1 - z2)
    Vec x1 y1 z1 * Vec x2 y2 z2 = Vec (x1 * x2) (y1 * y2) (z1 * z2)
    abs v = fmap abs v
    signum v = fmap signum v
    fromInteger n = from (fromInteger n)

instance NFData a => NFData (Vec a) where
    rnf (Vec x y z) = rnf x `seq` rnf y `seq` rnf z

type Vec3 = Vec Double
type Point3 = Vec Double

vec :: Num a => a -> a -> a -> Vec a
vec x y z = Vec x y z

from :: Num a => a -> Vec a
from n = vec n n n

zero :: Num a => Vec a
zero = from 0

one :: Num a => Vec a
one = from 1

dot :: Num a => Vec a -> Vec a -> a
dot (Vec x1 y1 z1) (Vec x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Num a => Vec a -> Vec a -> Vec a
cross (Vec x1 y1 z1) (Vec x2 y2 z2) = Vec x y z
     where
        x = y1 * z2 - z1 * y2
        y = z1 * x2 - x1 * z2
        z = x1 * y2 - y1 * x2

infixl 6 .+
(.+) :: (Num a) => Vec a -> a -> Vec a
(.+) v x = fmap (+x) v

infixl 6 .-
(.-) :: (Num a) => Vec a -> a -> Vec a
(.-) v x = fmap (`subtract` x) v

infixl 7 .*
(.*) :: (Num a) => Vec a -> a -> Vec a
(.*) v x = fmap (*x) v

infixl 7 ./
(./) :: (Fractional a) => Vec a -> a -> Vec a
(./) v x = fmap (/x) v

lenSquared :: Num a => Vec a -> a
lenSquared v = dot v v

len :: Vec Double -> Double
len = sqrt . lenSquared

unit :: Vec Double -> Vec Double
unit v = v ./ len v
