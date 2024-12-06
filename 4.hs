import Data.List (transpose, isPrefixOf, tails)
import qualified Data.Set as Set
import Data.Set (Set)

-- day 4
-- part 1:
-- we get a grid of letters, we need to find the count of occurences for XMAS within the grid, horisontally, vertically og any diagonal, both left, right, up and down.


-- Function to extract top-left to bottom-right diagonals
diagonalsTLBR :: [[a]] -> [[a]]
diagonalsTLBR [] = []
diagonalsTLBR xss = [diag i xss | i <- [0 .. (n + m - 2)]]
  where
    n = length xss
    m = length (head xss)
    diag k xss = [xss !! (k - j) !! j | j <- [max 0 (k - n + 1) .. min k (m - 1)], (k - j) < n, j < m]

diagTRBL :: [[a]] -> [[a]]
diagTRBL [] = []
diagTRBL xss = [diagTRBLFrom i xss | i <- [0 .. (n + m - 2)]]
    where
    n = length xss
    m = length (head xss)
    diagTRBLFrom k xss = [xss !! (j) !! (k - j) | j <- [max 0 (k - m + 1) .. min k (n - 1)], (k - j) >= 0, j < n]
    
    
    


extractAllDirectionalLines :: [String] -> [String]
extractAllDirectionalLines grid = do
    let transposedGrid = transpose grid
    let diagonalGrid = diagonalsTLBR grid
    let transposedDiagonalGrid = diagTRBL grid

    concat [grid, transposedGrid, diagonalGrid, transposedDiagonalGrid]

isPrefixedXMAS :: String -> Bool
isPrefixedXMAS s = (isPrefixOf "XMAS" s) || (isPrefixOf "SAMX" s)

countSubstring :: String -> Int
countSubstring str = length $ filter isPrefixedXMAS (tails str)
    
countAllSubstrings :: [String] -> Int
countAllSubstrings grid = do
    let allLines = extractAllDirectionalLines grid
    sum $ map countSubstring allLines

main :: IO ()
main = do
    content <- readFile "4.txt"
    let contentLines = lines content

   -- mapM_ print (extractAllDirectionalLines contentLines)

    print $ countAllSubstrings contentLines
    print $ length contentLines -- 10x10

    let lines = extractAllDirectionalLines contentLines
    print $ length lines -- 58 -> 10 + 10 + 19 + 19 
    -- diag should be 2n - 1

    let setLines = Set.fromList lines
    print $ length lines