module Rand (genNpuzzle) where

import System.Random
import System.Random.Shuffle

genNpuzzle :: Int -> IO [Int]
genNpuzzle n = do
            gen <- getStdGen
            return $ shuffle' [0 .. len - 1] len gen
            where len = n * n
