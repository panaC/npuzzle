module Astar where

import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.HashMap.Strict as Map
import Heurestic(manhatan)

run :: [Int] -> Maybe [Int]
run startPuzzle = 
  astar
    (PQ.singleton (manhatan startPuzzle) startPuzzle) --opened
    (Map.empty) --closed
    (Map.singleton startPuzzle 0) --gscore
    (Map.singleton startPuzzle $ manhatan startPuzzle) --fscore

astar :: MinPQueue Int [Int] -> HashMap [Int] [Int] -> HashMap [Int] Int -> HashMap [Int] Int-> Maybe [Int]
astar opened closed gscore fscore
  | PQ.null opened = Nothing
  | manhatan list == 0 = Just(list)
  | otherwise = astar newOpened newClosed newGscore newFscore
  where current = PQ.findMin opened
        list = snd current
        openedDeleteMin = PQ.deleteMin opened
        filteredNeighbor = filter (\x -> tentativeGscore < getMap x gscore) $ move4ways current
        filteredNeighborNotInOpened = filter
                                        (\x ->
                                          length (PQ.filterWithKey (\k a -> x == k) openedDeleteMin) >= 0)
                                        filteredNeighbor
        newOpened = fold (\x -> PQ.insert x openedDeleteMin) filteredNeighborNotInOpened
        newClosed = fold (\x -> PQ.insert x closed) filteredNeighbor
        newGscore = fold (\x -> PQ.insert x gscore) filteredNeighbor
        newFscore = fold (\x -> PQ.insert x fscore) filteredNeighbor
        tentativeGscore = (+1) Map.lookup current gscore

getMap k hashMap =
  | value == Nothing = maxBound :: Int
  | otherwise = value
  where value = Map.lookup k hashMap
