import Data.List (transpose, isPrefixOf, tails)

-- day 4
-- part 1:
-- we get a grid of letters, we need to find the count of occurences for XMAS within the grid, horisontally, vertically og any diagonal, both left, right, up and down.


-- Function to extract top-left to bottom-right diagonals
diagonalsTLBR :: [[a]] -> [[a]]
diagonalsTLBR [] = []
diagonalsTLBR xss = [diag i xss | i <- [0..(n + m - 2)]]
  where
    n = length xss
    m = length (head xss)
    diag k xss = [xss !! (k - j) !! j | j <- [max 0 (k - n + 1) .. min k (m - 1)]]

extractAllDirectionalLines :: [String] -> [String]
extractAllDirectionalLines grid = do
    let transposedGrid = transpose grid
    let diagonalGrid = diagonalsTLBR grid
    let transposedDiagonalGrid = diagonalsTLBR (transpose grid)

    let allDirections = concat [grid, transposedGrid, diagonalGrid, transposedDiagonalGrid]
    -- include reverse as well since that is part of word search
    concat [allDirections, (map reverse allDirections)]

isPrefixedXMAS :: String -> Bool
isPrefixedXMAS s = isPrefixOf "XMAS" s

countSubstring :: String -> Int
countSubstring str = length $ filter isPrefixedXMAS (tails str)
    
countAllSubstrings :: [String] -> Int
countAllSubstrings grid = do
    let allLines = extractAllDirectionalLines grid
    sum $ map countSubstring allLines

main :: IO ()
main = do
    -- Load file content and parse each line with the parser
    content <- readFile "4.txt"
    let contentLines = lines content

    print $ countAllSubstrings contentLines
