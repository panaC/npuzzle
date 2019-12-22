module Main where

import Rand
import Heurestic

main = do
  res <- Rand.genNpuzzle 3
  print $ res
  print $ Heurestic.manhatan 3 res
