module Goal (spiral) where

import Data.List
import Control.Monad

spiral n =
  replace $ grade . scanl1 (+) . concat $ zipWith replicate (counts n) (values n)
  where grade xs = map snd . sort $ zip xs [1..]
        values n = cycle [1,n,-1,-n]
        counts n = (n:) . concatMap ((<*>) (:) return) $ [n-1,n-2..1]
        len = n * n
        replace [] = []
        replace (x:xs)
          | x == len = 0:xs
          | otherwise = x:replace xs