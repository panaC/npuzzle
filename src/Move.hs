module Move where

zeroIndex :: [Int] -> Int
zeroIndex list = head [i | (x,i) <- zip list [0..], x == 0]

swapIndex :: Int -> [Int] -> [(Int, Int)]
swapIndex zeroIdx list = zip (
  filter (\x -> x >= 0 && x < (length list)) $
    map (+zeroIdx) [-3, 1, 3, -1]) (repeat zeroIdx)

swap :: Int -> Int -> [Int] -> [Int]
swap moveIdx zeroIdx list =
  let (i, j) =
        if moveIdx > zeroIdx
          then (zeroIdx, moveIdx)
          else (moveIdx, zeroIdx)
      left = take i list
      middle = take (j - i - 1) $ drop (i + 1) list
      right = drop (j + 1) list
    in left ++ [list !! j] ++ middle ++ [list !! i] ++ right

move4Ways :: [Int] -> [[Int]]
move4Ways list = [ swap i j list | (i, j) <- swapIndex (zeroIndex list) list]