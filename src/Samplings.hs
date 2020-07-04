
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Samplings where

import           System.Random
import           Data.List     (foldl')
import           Data.Maybe  (fromMaybe, isJust, fromJust)

import Vec
import BaseVec

sampleFraction :: RandomGen g => g -> (Double, g)
sampleFraction g = randomR (0, 1) g

sampleFractions :: RandomGen g => g -> Int -> ([Double], g)
sampleFractions g n = foldl' gen ([], g) [1..n]
    where
        gen (xs, g) _ = (x:xs, g')
            where 
                (x, g') = sampleFraction g

sampleBetween :: RandomGen g => g -> Double -> Double -> (Double, g)
sampleBetween g a b = (r * (b - a), g')
    where
        (r, g') = sampleFraction g

sampleBetweens :: RandomGen g => g -> Double -> Double -> Int -> ([Double], g)
sampleBetweens g a b n = (fmap (* (b - a)) xs, g')
    where
        (xs, g') = sampleFractions g n

samplePoint :: RandomGen g => g -> (Point3, g)
samplePoint g = (BaseVec xy, g')
    where
        (xy, g') = sampleFractions g 3  -- TODO Non-exhaustive patterns in function y

samplePointBetween :: RandomGen g => g -> Double -> Double -> (Point3, g)
samplePointBetween g a b = (BaseVec xy, g')
    where
        (xy, g') = sampleBetweens g a b 3 -- TODO : 2 causes Non-exhaustive patterns in function y

samplePointInCircle :: RandomGen g => g -> Double -> (Point3, g)
samplePointInCircle g radius = fromJust (find g) 
    where
        find g = p
            where
                (maybeP, g') = maybePoint g
                p = if isJust maybeP 
                       then Just (fromJust maybeP, g')
                        else find g'

        maybePoint g = (maybeP, g')
            where
                (p, g') = samplePointBetween g (-radius) radius 
                maybeP = if (len2 p) >= radius then Just p else Nothing 
                
-- samplePointInCircle :: RandomGen g => g -> Double -> (Point3, g)
-- samplePointInCircle g radius = (head points, g')
--     where
--         (points, g') = foldl makeCircle ([], g) [0..]
--         makeCircle (circles, g) _ = (fromMaybe circles (fmap (:circles) c), g')
--             where
--                 (p, g') = samplePointBetween g (-radius) radius 
--                 c = if (len2 p) >= radius then Just p else Nothing
--                 

-- class Sampling sampler where
--     -- type SamplerMonad sampler :: * -> * 
--     -- sample :: (SamplerMonad sampler) Double
--     -- between :: Double -> Double -> (SamplerMonad sampler) Double
--     type SamplerMonad sampler 
--     sample :: Double
--     between :: Double -> Double -> Double
-- 
-- 
-- newtype StdSampler = StdSampler StdGen  
--     -- let gen = mkStdGen 22
--     -- let randList = (randomRs (0,1) gen) :: [Double]
-- instance Sampling StdSampler where
--     type SamplerMonad StdSampler = Do
--     sample = undefined
--     between a b = undefined
