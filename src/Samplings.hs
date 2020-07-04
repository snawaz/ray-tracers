
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Samplings where

import           System.Random

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
