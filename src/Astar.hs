module Astar where

import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.HashMap.Strict as Map
import Heurestic(manhatan)
import Move(move4Ways)
import Data.Maybe(fromMaybe)

heurestic :: Int -> [Int] -> Int
heurestic n puzzle = manhatan n puzzle

run :: Int -> [Int] -> Maybe [Int]
run n startPuzzle = 
  let 
    opened = PQ.singleton h startPuzzle
    closed = Map.empty
    gscore = Map.singleton startPuzzle 0
    fscore = Map.singleton startPuzzle h
    h = heurestic n startPuzzle
  in astar n opened closed gscore fscore

astar :: Int -> PQ.MinPQueue Int [Int] -> Map.HashMap [Int] [Int] -> Map.HashMap [Int] Int -> Map.HashMap [Int] Int-> Maybe [Int]
astar n opened closed gscore fscore
  | PQ.null opened = Nothing
  | heurestic n puzzle == 0 = Just(puzzle)
  | otherwise = astar n newOpened newClosed newGscore newFscore
  where current = PQ.findMin opened
        puzzle = snd current
        openedDeleteMin = PQ.deleteMin opened
        filteredNeighbor = filter (\x -> tentativeGscore < getMap x gscore) $ move4Ways puzzle
        filteredNeighborNotInOpened = filter
                                        (\x ->
                                          PQ.size (PQ.filter (\a -> x == a) openedDeleteMin) == 0)
                                        filteredNeighbor
        newOpened = foldr (\x -> PQ.insert (heurestic n x) x) openedDeleteMin filteredNeighborNotInOpened
        newClosed = foldr (\x -> Map.insert x puzzle) closed filteredNeighbor
        newGscore = foldr (\x -> Map.insert x tentativeGscore) gscore filteredNeighbor
        newFscore = foldr (\x -> Map.insert x (tentativeGscore + heurestic n x)) fscore filteredNeighbor
        tentativeGscore = (+1) $ getMap puzzle gscore

getMap :: [Int] -> Map.HashMap [Int] Int -> Int
getMap k hashMap = fromMaybe (maxBound :: Int) $ Map.lookup k hashMap
