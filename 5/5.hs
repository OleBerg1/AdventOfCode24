import Data.List.Split
import Data.List ((\\))

-- DAY 5:
-- input two part 
-- 1: Int|Int -> (x, y)
-- 2: [Int]

-- 1. is set of rules, y CANNOT come after x
-- extract middle value of each line that does not break the rules, and find sum.

splitAtEmptyLine :: String -> (String, String)
splitAtEmptyLine input =
    let (before, after) = break (== "") (lines input)
    in (unlines before, unlines (drop 1 after))

stringToPair :: String -> (Int, Int)
stringToPair str =
    let (x, _:y) = break (== '|') str
    in (read x, read y) 

parseRules :: [String] -> [(Int, Int)]
parseRules rules = map stringToPair rules

parseList :: String -> [Int]
parseList str = map (\x -> read x) (splitOn "," str)

isHeadLegal :: [(Int, Int)] -> [Int] -> Bool
isHeadLegal rules line = 
    let relevantRules = filter (\(_, x) -> head line == x) rules
        checkRule (x, _) = not $ elem x (tail line)
    in all checkRule relevantRules


-- Can be optimized by making it tailrecursive by removing if from returned expression
isLineLegal :: [(Int, Int)] -> [Int] -> Bool
isLineLegal rules [] = True
isLineLegal rules line = if isHeadLegal rules line
    then isLineLegal rules (tail line)
    else False
    
extractMiddleElement :: [Int] -> Int
extractMiddleElement lst = lst !! (length lst `div` 2)

partitionByRules :: [(Int, Int)] -> Int -> [Int] -> ([Int], [Int])
partitionByRules rules pivot xs =
    (filter (\x -> (x, pivot) `notElem` rules) xs,  -- Can go before pivot
    filter (\x -> (pivot, x) `notElem` rules) xs) -- Must go after pivot

sortLineQuicksort :: [(Int, Int)] -> [Int] -> [Int]
sortLineQuicksort _ [] = []
sortLineQuicksort rules (p:xs) =
    let (left, right) = partitionByRules rules p xs
    in sortLineQuicksort rules left ++ [p] ++ sortLineQuicksort rules right

-- We need to split into two parts: before and after empty newline
main :: IO ()
main = do
    content <- readFile "5.txt"
    let (part1, part2) = splitAtEmptyLine content

     -- Print the two parts to verify
    -- putStrLn "Part 1:"
    -- putStrLn part1
    -- putStrLn "Part 2:"
    -- putStrLn part2

    let parsedRules = parseRules $ lines part1
    let parsedLines = map parseList (lines part2)

    let legalLines = filter (isLineLegal parsedRules) parsedLines
    let sumMiddleLegalValues = sum $ map extractMiddleElement legalLines
    print $ "Legal values: " ++ show sumMiddleLegalValues

    -- part 2
    let notLegalLines = filter (\line -> not $ isLineLegal parsedRules line) parsedLines
    

    let sortedLines = map reverse (map (sortLineQuicksort parsedRules) notLegalLines) -- bug in sorting algorithm that sorts it opposite way, just reversing instead of spending time thinking

    let sortedMiddleValues = sum $ map extractMiddleElement sortedLines
    print $ "Sorted values: " ++ show sortedMiddleValues