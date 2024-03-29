module Astar (astar) where

import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.HashMap.Strict as Map
import Heurestic(manhatan)
import Move(move4Ways)
import Data.Maybe(fromMaybe)
import Debug.Trace(trace)

heurestic :: Int -> [Int] -> Int
heurestic n puzzle = manhatan n puzzle

run :: Int -> [Int] -> Maybe (Int, Int, [[Int]], Int, [Int])
run n startPuzzle = 
  let 
    opened = PQ.singleton h startPuzzle
    closed = Map.empty
    gscore = Map.singleton startPuzzle 0
    fscore = Map.singleton startPuzzle h
    h = heurestic n startPuzzle
  in astar n opened closed gscore fscore 0 0

astar :: Int -> PQ.MinPQueue Int [Int] -> Map.HashMap [Int] [Int] -> Map.HashMap [Int] Int -> Map.HashMap [Int] Int -> Int -> Int -> Maybe (Int, Int, [[Int]], Int, [Int])
astar n opened closed gscore fscore timecomp spacecomp
  | PQ.null opened = Nothing
  | heurestic n puzzle == 0 = Just(timecomp, spacecomp, (reconstructPath puzzle []), length (reconstructPath puzzle []), puzzle)
  | otherwise = astar n newOpened newClosed newGscore newFscore newTimeComp newSpaceComp
  where current = trace (show $ PQ.size opened) PQ.findMin opened
        puzzle = snd current
        openedDeleteMin = PQ.deleteMin opened
        filteredNeighbor = filter (\x -> tentativeGscore < getMap x gscore) $ move4Ways puzzle
        filteredNeighborNotInOpened = filter
                                        (\x ->
                                          PQ.size (PQ.filter (\a -> x == a) openedDeleteMin) == 0)
                                        filteredNeighbor
        newTimeComp = (+timecomp) $ length filteredNeighborNotInOpened
        newSpaceComp = max spacecomp $ PQ.size opened
        newOpened = foldr (\x -> PQ.insert (heurestic n x) x) openedDeleteMin filteredNeighborNotInOpened
        newClosed = foldr (\x -> Map.insert x puzzle) closed filteredNeighbor
        newGscore = foldr (\x -> Map.insert x tentativeGscore) gscore filteredNeighbor
        newFscore = foldr (\x -> Map.insert x (tentativeGscore + heurestic n x)) fscore filteredNeighbor
        tentativeGscore = (+1) $ getMap puzzle gscore
        reconstructPath current path = if Map.lookup current closed /= Nothing
                                        then reconstructPath (fromMaybe [] (Map.lookup current closed)) (current : path)
                                        else current : path

getMap :: [Int] -> Map.HashMap [Int] Int -> Int
getMap k hashMap = fromMaybe (maxBound :: Int) $ Map.lookup k hashMap
