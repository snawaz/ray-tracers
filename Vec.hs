
{-# LANGUAGE NamedFieldPuns #-}

module Vec where

data Vec3 = Vec3 {
        x :: Double,
        y :: Double,
        z :: Double
    } deriving (Show)

toVec :: Double -> Double -> Double -> Vec3
toVec x y z = Vec3 { x, y, z }

toList :: Vec3 -> [Double]
toList Vec3{x, y, z} = [x, y, z]

fromList :: [Double] -> Vec3
fromList [x, y, z] = toVec x y z

instance Num Vec3 where
    v1 + v2 = fromList $ zipWith (+) (toList v1) (toList v2)
    v1 * v2 = fromList $ zipWith (*) (toList v1) (toList v2)
    v1 - v2 = fromList $ zipWith (-) (toList v1) (toList v2)
    abs    v = fromList $ fmap abs (toList v)
    signum v = fromList $ fmap signum (toList v)
    fromInteger n = fromList $ replicate 3 (fromInteger n)

dot :: Vec3 -> Vec3 -> Double
dot v1 v2 = sum $ zipWith (*) (toList v1) (toList v2)

apply :: (Double -> Double) -> Vec3 ->  Vec3
apply f v = fromList $ fmap f (toList v)

len2 :: Vec3 -> Double
len2 v = dot v v

len :: Vec3 -> Double
len = sqrt . len2
