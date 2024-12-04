getAllTranspositions :: [String] -> [[String]]
getAllTranspositions grid = do
    let diagonals = diagTRBL grid
    diagonals


main :: IO ()
main = do
    -- Load file content and parse each line with the parser
    content <- readFile "4t.txt"
    let contentLines = lines content

    mapM_ putStrLn contentLines
    mapM_ (diagTRBL contentLines)

