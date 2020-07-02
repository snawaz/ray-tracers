
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module BaseVec where

import qualified Prelude as P
import Prelude hiding ()

import Utils

arity = 3

data BaseVec a = BaseVec [a] deriving(Show, Functor)

instance Num a => Num (BaseVec a) where
    BaseVec v1 + BaseVec v2 = BaseVec $ zipWith (+) v1 v2 
    BaseVec v1 * BaseVec v2 = BaseVec $ zipWith (-) a b 
        where 
            a = zipWith (*) (rotate 1 v1) (rotate 2 v2)
            b = zipWith (*) (rotate 2 v1) (rotate 1 v2)
    BaseVec v1 - BaseVec v2 = BaseVec $ zipWith (-) v1 v2
    abs (BaseVec v) = BaseVec $ fmap abs v
    signum (BaseVec v) = BaseVec $ fmap signum v
    fromInteger n = BaseVec $ replicate arity (fromInteger n)

vec :: (Num a) => a -> a -> a -> BaseVec a
vec x y z = BaseVec [x, y, z]

from :: (Num a) => a -> BaseVec a
from n = vec n n n
zero = from 0 
one = from 1 

x (BaseVec [x', _, _]) = x'
y (BaseVec [_, y', _]) = y'
z (BaseVec [_, _, z']) = z'

type Kolor  = BaseVec Int
type Vector = BaseVec Double 

dot :: Num a => BaseVec a -> BaseVec a -> a
dot (BaseVec v1) (BaseVec v2) = sum $ zipWith (*) v1 v2

-- class BinaryOperation a b where
--     type Result a b
--     (<.>) :: a -> b -> Result a b
--     (<.>) a b = a BaseVec.* b
--     (<.*>) :: a -> b -> Result a b
--     (<.*>) a b = a BaseVec.+ b
--     (*) :: a -> b -> Result a b
--     (+) :: a -> b -> Result a b
--     (-) :: a -> b -> Result a b
--     (/) :: a -> b -> Result a b
-- 
-- instance BinaryOperation (BaseVec Int) Int where
--     type Result (BaseVec Int) Int = BaseVec Int
--     (*) (BaseVec xs) x = BaseVec $ fmap (P.*x) xs   
--     (+) (BaseVec xs) x = BaseVec $ fmap (P.+x) xs   
--     (-) (BaseVec xs) x = BaseVec $ fmap (P.-x) xs   
--     (/) (BaseVec xs) x = BaseVec $ fmap (`div` x) xs   
-- 
-- instance BinaryOperation Int (BaseVec Int) where
--     type Result Int (BaseVec Int) = BaseVec Int
--     (*) x (BaseVec xs) = BaseVec $ fmap (P.*x) xs   
--     (+) x (BaseVec xs) = BaseVec $ fmap (P.+x) xs   
--     (-) x (BaseVec xs) = BaseVec $ fmap (x P.-) xs   
--     (/) x (BaseVec xs) = BaseVec $ fmap (x `div`) xs   
-- 
-- instance BinaryOperation (BaseVec Int) (BaseVec Int) where
--     type Result (BaseVec Int) (BaseVec Int) = BaseVec Int
--     (*) (BaseVec xs1) (BaseVec xs2) = BaseVec $ zipWith (P.*) xs1 xs2 
--     (+) (BaseVec xs1) (BaseVec xs2) = BaseVec $ zipWith (P.+) xs1 xs2  
--     (-) (BaseVec xs1) (BaseVec xs2) = BaseVec $ zipWith (P.-) xs1 xs2  
--     (/) (BaseVec xs1) (BaseVec xs2) = BaseVec $ zipWith div xs1 xs2  
-- 
-- instance BinaryOperation (BaseVec Double) Double where
--     type Result (BaseVec Double) Double = BaseVec Double
--     (*) (BaseVec xs) x = BaseVec $ fmap (P.*x) xs   
--     (+) (BaseVec xs) x = BaseVec $ fmap (P.+x) xs   
--     (-) (BaseVec xs) x = BaseVec $ fmap (P.-x) xs   
--     (/) (BaseVec xs) x = BaseVec $ fmap (P./x) xs   
-- 
-- instance BinaryOperation Double (BaseVec Double) where
--     type Result Double (BaseVec Double) = BaseVec Double
--     (*) x (BaseVec xs) = BaseVec $ fmap (P.*x) xs   
--     (+) x (BaseVec xs) = BaseVec $ fmap (P.+x) xs   
--     (-) x (BaseVec xs) = BaseVec $ fmap (x P.-) xs   
--     (/) x (BaseVec xs) = BaseVec $ fmap (x P./) xs   
-- 
-- instance BinaryOperation (BaseVec Double) (BaseVec Double) where
--     type Result (BaseVec Double) (BaseVec Double) = BaseVec Double
--     (*) (BaseVec xs1) (BaseVec xs2) = BaseVec $ zipWith (P.*) xs1 xs2 
--     (+) (BaseVec xs1) (BaseVec xs2) = BaseVec $ zipWith (P.+) xs1 xs2  
--     (-) (BaseVec xs1) (BaseVec xs2) = BaseVec $ zipWith (P.-) xs1 xs2  
--     (/) (BaseVec xs1) (BaseVec xs2) = BaseVec $ zipWith (P./) xs1 xs2  
