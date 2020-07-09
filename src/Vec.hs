
{-# LANGUAGE DeriveFunctor #-}

module Vec(
    Vec (Vec),
    zero, one, from ,vec,
    xCoor, yCoor, zCoor,
    dot, cross, unit,
    (.+), (.-), (.*), (./),
    Point3, Vec3
)where

import           Utils (rotate)

arity :: Int
arity = 3

data Vec a = Vec [a] deriving(Show, Functor)

type Vec3 = Vec Double
type Point3 = Vec Double

instance Num a => Num (Vec a) where
    Vec v1 + Vec v2 = Vec $ zipWith (+) v1 v2
    Vec v1 - Vec v2 = Vec $ zipWith (-) v1 v2
    Vec v1 * Vec v2 = Vec $ zipWith (*) v1 v2
    abs (Vec v) = Vec $ fmap abs v
    signum (Vec v) = Vec $ fmap signum v
    fromInteger n = Vec $ replicate arity (fromInteger n)

vec :: Num a => a -> a -> a -> Vec a
vec x y z = Vec [x, y, z]

from :: Num a => a -> Vec a
from n = vec n n n

zero :: Num a => Vec a
zero = from 0

one :: Num a => Vec a
one = from 1

xCoor :: Num a => Vec a -> a
xCoor (Vec [x', _, _]) = x'
xCoor _                = undefined

yCoor :: Num a => Vec a -> a
yCoor (Vec [_, y', _]) = y'
yCoor _                = undefined

zCoor :: Num a => Vec a -> a
zCoor (Vec [_, _, z']) = z'
zCoor _                = undefined

dot :: Num a => Vec a -> Vec a -> a
dot (Vec v1) (Vec v2) = sum $ zipWith (*) v1 v2

cross :: Num a => Vec a -> Vec a -> Vec a
cross (Vec v1) (Vec v2) = Vec $ zipWith (-) a b
    where
        a = zipWith (*) (rotate 1 v1) (rotate 2 v2)
        b = zipWith (*) (rotate 2 v1) (rotate 1 v2)

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
