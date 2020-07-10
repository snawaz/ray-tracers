


module Main(main) where

import           Image  (writeImage)
import           Scenes (randomScene)

main :: IO ()
main = do
    writeImage 100 100 (randomScene 22) 50
