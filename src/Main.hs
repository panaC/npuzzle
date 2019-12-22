module Main where

import Rand

main = do
  res <- Rand.genNpuzzle 3
  print $ res
