module Rand (genNpuzzle) where

import System.Random

genNpuzzle :: Int -> IO [Int]
genNpuzzle n = do
            gen <- getStdGen
            return $ take (n * n) (randomRs (1, (n * n)) gen)
