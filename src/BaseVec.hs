
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE IncoherentInstances    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module BaseVec where

import           Prelude hiding ()
import qualified Prelude as P
import Control.DeepSeq

import           Utils

arity = 3

data BaseVec a = BaseVec [a] deriving(Show, Functor)

instance Num a => Num (BaseVec a) where
    BaseVec v1 + BaseVec v2 = BaseVec $ zipWith (+) v1 v2
    BaseVec v1 * BaseVec v2 = BaseVec $ zipWith (*) v1 v2
    BaseVec v1 - BaseVec v2 = BaseVec $ zipWith (-) v1 v2
    abs v = fmap abs v
    signum v = fmap signum v
    fromInteger n = BaseVec $ replicate arity (fromInteger n)

instance NFData a => NFData (BaseVec a) where
    rnf (BaseVec xs) = rnf xs

vec :: (Num a) => a -> a -> a -> BaseVec a
vec x y z = BaseVec [x, y, z]

from :: (Num a) => a -> BaseVec a
from n = vec n n n

zero :: (Num a) => BaseVec a
zero = from 0

one :: (Num a) => BaseVec a
one = from 1

x, y, z :: BaseVec Double -> Double
x (BaseVec [x', _, _]) = x'
y (BaseVec [_, y', _]) = y'
z (BaseVec [_, _, z']) = z'

dot :: Num a => BaseVec a -> BaseVec a -> a
dot (BaseVec v1) (BaseVec v2) = sum $ zipWith (*) v1 v2

cross :: Num a => BaseVec a -> BaseVec a -> BaseVec a
cross (BaseVec v1) (BaseVec v2) = BaseVec $ zipWith (-) a b
    where
        a = zipWith (*) (rotate 1 v1) (rotate 2 v2)
        b = zipWith (*) (rotate 2 v1) (rotate 1 v2)

-- https://bugfactory.io/blog/custom-infix-operators-in-haskell/
-- https://www.haskell.org/onlinereport/decls.html#fixity

infixl 6 .+
(.+) :: (Num a) => BaseVec a -> a -> BaseVec a
(.+) v x = fmap (+x) v

infixl 6 .-
(.-) :: (Num a) => BaseVec a -> a -> BaseVec a
(.-) v x = fmap (`subtract` x) v

infixl 7 .*
(.*) :: (Num a) => BaseVec a -> a -> BaseVec a
(.*) v x = fmap (*x) v

infixl 7 ./
(./) :: (Fractional a) => BaseVec a -> a -> BaseVec a
(./) v x = fmap (/x) v

-- class ScalarOps a where
--     data MyVec a :: *
--     (.+) :: a -> b -> c
--
-- instance (Num a) => ScalarOps (BaseVec a) a (BaseVec a) where
--     (.+) v x = fmap (+x) v
--
-- instance (Num a) => ScalarOps a (BaseVec a) (BaseVec a) where
--     (.+) x v = fmap (x+) v

-- class ScalarOps a b c | c -> a where
--     (.+) :: a -> b -> c
--
-- instance (Num a) => ScalarOps (BaseVec a) a (BaseVec a) where
--     (.+) v x = fmap (+x) v
--
-- instance (Num a) => ScalarOps a (BaseVec a) (BaseVec a) where
--     (.+) x v = fmap (x+) v

-- class ScalarOps a b c | a b -> c where
--     (.+) :: a -> b -> c
--
-- instance (Num a) => ScalarOps (BaseVec a) a (BaseVec a) where
--     (.+) v x = fmap (+x) v
--
-- instance (Num a) => ScalarOps a (BaseVec a) (BaseVec a) where
--     (.+) x v = fmap (x+) v

-- class BinaryOperation a b where
--     type Result a b
--     (.*) :: a -> b -> Result a b
--     (.+) :: a -> b -> Result a b
--     (.-) :: a -> b -> Result a b
--     (./) :: a -> b -> Result a b
--
-- instance BinaryOperation (BaseVec Int) Int where
--     type Result (BaseVec Int) Int = BaseVec Int
--     (.*) v x = fmap (*x) v
--     (.+) v x = fmap (+x) v
--     (.-) v x = fmap (`subtract` x) v
--     (./) v x = fmap (`div` x) v
--
-- instance BinaryOperation Int (BaseVec Int) where
--     type Result Int (BaseVec Int) = BaseVec Int
--     (.*) x v = fmap (*x) v
--     (.+) x v = fmap (+x) v
--     (.-) x v = fmap (x-) v
--     (./) x v = fmap (x `div`) v
--
-- instance BinaryOperation (BaseVec Double) Double where
--     type Result (BaseVec Double) Double = BaseVec Double
--     (.*) v x = fmap (*x) v
--     (.+) v x = fmap (+x) v
--     (.-) v x = fmap (`subtract` x) v
--     (./) v x = fmap (/x) v
--
-- instance BinaryOperation Double (BaseVec Double) where
--     type Result Double (BaseVec Double) = BaseVec Double
--     (.*) x v = fmap (*x) v
--     (.+) x v = fmap (+x) v
--     (.-) x v = fmap (x-) v
--     (./) x v = fmap (x/) v
