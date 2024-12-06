-- DAY 4 part 2:
-- Find occurances of MAS or SAM overlapping in an X 

-- Can be vastly optimized by checking letters before making grids, but why spend time on that when runs fine without




extract3x3Grids :: [[a]] -> [[[a]]]
extract3x3Grids grid = -- Extracts all 3x3 subgrids of a greater grid
  [ [take 3 (drop c row) | row <- take 3 (drop r grid)] 
  | r <- [0..length grid - 3], c <- [0..length (head grid) - 3] ]

checkDiagonals :: [String] -> Bool
checkDiagonals grid =
    (forwardDiagonal == "SAM" || forwardDiagonal == "MAS") &&
    (backwardDiagonal == "SAM" || backwardDiagonal == "MAS")
    where
        forwardDiagonal = [grid !! 0 !! 0, grid !! 1 !! 1, grid !! 2 !! 2]
        backwardDiagonal = [grid !! 2 !! 0, grid !! 1 !! 1, grid !! 0 !! 2]

main :: IO ()
main = do
    content <- readFile "4.txt"
    let contentLines = lines content

    let allSubGrids = extract3x3Grids contentLines

    print $ length allSubGrids

    print $ length $ filter checkDiagonals allSubGrids