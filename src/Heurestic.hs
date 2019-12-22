
module Heurestic (manhatan) where

import Goal

manhatan :: Int -> [Int] -> Int
manhatan n list
  | n * n == length list = sum [ abs $ x - y | (x, y) <- zip list (Goal.spiral n)]
  | otherwise = error "bad list length"