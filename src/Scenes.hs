
module Scenes(randomScene) where

import           Data.List     (foldl')
import           Data.Maybe    (fromMaybe)
import           System.Random (mkStdGen)

import           Hittable      (Dielectric (Dielectric), HittableList (HittableList), Lambertian (Lambertian), Material (Material), Metal (Metal),
                                Sphere (Sphere), toHittableList)
import           Samplings     (sampleFraction, sampleFractions, samplePoints)
import           Vec           (len, vec, from)

randomScene :: Int -> HittableList Sphere
randomScene seed = toHittableList allObjects
    where
        allObjects = [groundObject] ++ randomObjects ++ fixedObjects
        groundObject = Sphere (vec 0 (-1000) 0) 1000 (Material (Lambertian (from 0.5)))
        randomObjects = fst $ foldl' mkObject ([], mkStdGen seed) $ (,) <$> [-11..11] <*> [-11..11]
        mkObject (objects, g) (a, b) = (fromMaybe objects (fmap (\o-> o:objects) object), g2)
            where
                ([chooseMat, x, z], g1) = sampleFractions g 3
                center = vec (a + 0.9 * x) 0.2 (b + 0.9 * z)
                (object, g2) = if len (center - vec 4 0.2 0) > 0.9 then createRandomObject g1 else (Nothing, g1)
                createRandomObject g = (Just obj, g'')
                    where
                        ([p1, p2], g') = samplePoints g 2
                        (d1, g'') = sampleFraction g'
                        mat = if chooseMat < 0.8
                                 then Material $ Lambertian (p1 * p2)
                                 else if chooseMat < 0.95
                                    then Material $ Metal (fmap (\i -> 0.5 + 0.5 * i) p1) (0.5 * d1)
                                    else Material $ Dielectric 1.5
                        obj = Sphere center 0.2 mat
        fixedObjects =  [
                (Sphere (vec 0 1 0) 1.0 (Material (Dielectric 1.5))),
                (Sphere (vec (-4) 1 0) 1.0 (Material (Lambertian (vec 0.4 0.2 0.1)))),
                (Sphere (vec 4 1 0) 1.0 (Material (Metal (vec 0.7 0.6 0.5) 0.0)))
            ]
        -- allObjects = [groundObject] ++ randomObjects ++ fixedObjects
        -- groundObject = Sphere (vec 0 (-1000) 0) 1000 (Material (Lambertian (from 0.5)))
        -- randomObjects = fst $ foldl' mkObject ([], mkStdGen seed) $ (,) <$> [-11..11] <*> [-11..11]
        -- mkObject (objects, g) (a, b) = (fromMaybe objects (fmap (\o-> o:objects) object), g2)
        --     where
        --         ([chooseMat, x, z], g1) = sampleFractions g 3
        --         center = vec (a + 0.9 * x) 0.2 (b + 0.9 * z)
        --         (object, g2) = if len (center - vec 4 0.2 0) > 0.9 then createRandomObject g1 else (Nothing, g1)
        --         createRandomObject g3 = (Just obj, g5)
        --             where
        --                 ([p1, p2], g4) = samplePoints g3 2
        --                 (d1, g5) = sampleFraction g4
        --                 mat = if chooseMat < 0.8
        --                          then Material $ Lambertian (p1 * p2)
        --                          else if chooseMat < 0.95
        --                             then Material $ Metal (fmap (\i -> 0.5 + 0.5 * i) p1) (0.5 * d1)
        --                             else Material $ Dielectric 1.5
        --                 obj = Sphere center 0.2 mat
        -- fixedObjects =  [
        --         (Sphere (vec 0 1 0) 1.0 (Material (Dielectric 1.5))),
        --         (Sphere (vec (-4) 1 0) 1.0 (Material (Lambertian (vec 0.4 0.2 0.1)))),
        --         (Sphere (vec 4 1 0) 1.0 (Material (Metal (vec 0.7 0.6 0.5) 0.0)))
        --     ]


