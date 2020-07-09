{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE IncoherentInstances    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeSynonymInstances   #-}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE MagicHash, UnboxedTuples #-}

module BaseVec where

import           Prelude hiding ()
import qualified Prelude as P
import Control.DeepSeq
import           Utils
import GHC.Prim -- http://hackage.haskell.org/package/ghc-prim-0.6.1/docs/GHC-Prim.html

arity = 3

data MyV = MyV {
    x' :: {-# UNPACK #-} !Double,
    y' :: {-# UNPACK #-} !Double,
    z' :: {-# UNPACK #-} !Double
    -- x' :: Double#,
    -- y' :: Double#,
    -- z' :: Double#
} deriving(Show)

instance Num MyV where
    {-# INLINE (+) #-}
    MyV x1 y1 z1 + MyV x2 y2 z2 = MyV (x1 + x2) (y1 + y2) (z1 + z2)
    {-# INLINE (-) #-}
    MyV x1 y1 z1 - MyV x2 y2 z2 = MyV (x1 - x2) (y1 - y2) (z1 - z2)
    {-# INLINE (*) #-}
    MyV x1 y1 z1 * MyV x2 y2 z2 = MyV (x1 * x2) (y1 * y2) (z1 * z2)
    abs v = undefined-- fmap abs v
    signum v = undefined -- fmap signum v
    fromInteger n = undefined -- from (fromInteger n)

instance NFData MyV  where
    rnf (MyV x y z) = rnf x `seq` rnf y `seq` rnf z

data BaseVec a = BaseVec {
    x :: !a,
    y :: !a,
    z :: !a
} deriving(Show, Functor)

-- instance Num a => Num (BaseVec a) where
instance Num (BaseVec Double) where
    {-# INLINE (+) #-}
    BaseVec x1 y1 z1 + BaseVec x2 y2 z2 = BaseVec (x1 + x2) (y1 + y2) (z1 + z2)
    {-# INLINE (-) #-}
    BaseVec x1 y1 z1 - BaseVec x2 y2 z2 = BaseVec (x1 - x2) (y1 - y2) (z1 - z2)
    {-# INLINE (*) #-}
    BaseVec x1 y1 z1 * BaseVec x2 y2 z2 = BaseVec (x1 * x2) (y1 * y2) (z1 * z2)
    abs v = fmap abs v
    signum v = fmap signum v
    fromInteger n = from (fromInteger n)

instance NFData a => NFData (BaseVec a) where
    rnf (BaseVec x y z) = rnf x `seq` rnf y `seq` rnf z

vec :: (Num a) => a -> a -> a -> BaseVec a
vec x y z = BaseVec x y z

from :: (Num a) => a -> BaseVec a
from n = vec n n n

zero :: (Num a) => BaseVec a
zero = from 0

one :: (Num a) => BaseVec a
one = from 1

-- x, y, z :: BaseVec Double -> Double
-- x (BaseVec [x', _, _]) = x'
-- y (BaseVec [_, y', _]) = y'
-- z (BaseVec [_, _, z']) = z'

-- ot :: Num a => BaseVec a -> BaseVec a -> a
--dot :: BaseVec Double -> BaseVec Double -> Double
{-# INLINE dot #-}
dot (BaseVec x1 y1 z1) (BaseVec x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

{-# INLINE cross #-}
cross :: Num a => BaseVec a -> BaseVec a -> BaseVec a
cross (BaseVec x1 y1 z1) (BaseVec x2 y2 z2) = BaseVec x y z
    where
        x = y1 * z2 - z1 * y2
        y = z1 * x2 - x1 * z2
        z = x1 * y2 - y1 * x2

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
