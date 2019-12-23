module Parser (parser) where

import Data.Maybe(fromMaybe)
import Data.List(elemIndex)

split :: Eq a => a -> [a] -> ([a], [a])
split chr str = splitAt (fromMaybe (length str) $ elemIndex chr str) str

lexer :: [Char] -> IO [[String]]
lexer filePath = do
      lexedPuzzle <- map words . filter (not . null) . map fst . map (split '#') . lines <$> readFile filePath
      return $ lexedPuzzle

parser :: [Char] -> IO (Maybe (Int, [Int]))
parser filePath = do
      lexedPuzzle <- lexer filePath
      let nStr = head $ head lexedPuzzle
      let n = read $ nStr :: Int
      let puzzleLine = drop 1 lexedPuzzle
      let puzzleList = foldr (++) [] puzzleLine
      let puzzle = map (\x -> read x :: Int) puzzleList
      let errorLine = length puzzleLine /= n
      let errorCol = not $ and $ map (\x -> length x == n) puzzleLine
      if errorLine then do
        return $ error $ "Wrong number of line, expected " ++ show n
      else if errorCol then do
        return $ error $ "Wrong number of column, expected " ++ show n
      else do
        return $ Just(n, puzzle)