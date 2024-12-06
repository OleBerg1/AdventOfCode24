import Data.List (transpose, isPrefixOf, tails)

-- day 4
-- part 1:
-- we get a grid of letters, we need to find the count of occurences for XMAS within the grid, horisontally, vertically og any diagonal, both left, right, up and down.

diagonals :: [[a]] -> [[a]]
diagonals [] = []
diagonals xss = [diag i xss | i <- [0..(n + m - 2)]]
  where
    n = length xss
    m = length (head xss)
    diag k xss = [xss !! (k - j) !! j | j <- [max 0 (k - n + 1) .. min k (m - 1)]]



extractAllDirectionalLines :: [String] -> [Int]
extractAllDirectionalLines grid = do
    let transposedGrid = transpose grid
    let diagonalGrid = diagonals grid
    let transposedDiagonalGrid = diagonals $ reverse grid

    [countAllSubstrings grid, countAllSubstrings transposedGrid, countAllSubstrings diagonalGrid, countAllSubstrings transposedDiagonalGrid]

isPrefixedXMAS :: String -> Bool
isPrefixedXMAS s = (isPrefixOf "XMAS" s) || (isPrefixOf "SAMX" s)

countSubstring :: String -> Int
countSubstring str = length $ filter isPrefixedXMAS (tails str)
    
countAllSubstrings :: [String] -> Int
countAllSubstrings grid = do
    sum $ map countSubstring grid

main :: IO ()
main = do
    content <- readFile "4.txt"
    let contentLines = lines content

   -- mapM_ print (extractAllDirectionalLines contentLines)
    mapM_ print (diagonals contentLines)
    print "---------------"
    mapM_ print (diagonals $ reverse contentLines)

    print $ "Occurances of XMAS: " ++ show (sum $ extractAllDirectionalLines contentLines)
