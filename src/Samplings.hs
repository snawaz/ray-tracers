
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Samplings where

import           System.Random

class Sampling sampler where
    type SamplerMonad sampler :: * -> *
    sample :: sampler -> (SamplerMonad sampler) Double
    between :: sampler -> Double -> Double -> (SamplerMonad sampler) Double



