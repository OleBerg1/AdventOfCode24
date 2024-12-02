-- Day 2
-- get reports, 1 line -> 1 report, each number is a level
-- x1 x2 x3
-- y1 y2 y3
-- z1 z2 z3
-- ...

-- report is only safe if each level gradually (in|de)crease 
-- ALL levels must be either increasing or decreasing 
-- any two adjecent levels can have a diff 0 > 4


-- PART 2
-- same tests, but tolerate single violation to requirements


import System.IO
import Data.List.Split (splitOn)

parseLine :: String -> [Int]
parseLine line = map read $ words line

isAscending :: (Ord a) => [a] -> Bool
isAscending xs = and $ zipWith (<=) xs (tail xs)

isDescending :: (Ord a) => [a] -> Bool
isDescending xs = and $ zipWith (>=) xs (tail xs)

isSortedEitherWay :: (Ord a) => [a] -> Bool
isSortedEitherWay xs = isAscending xs || isDescending xs

isGradual :: (Ord a, Num a) => [a] -> Bool
isGradual line = all (\(x, y) -> abs(x - y) > 0 && abs(x - y) < 4) $ zip line (tail line)

isValid :: (Ord a, Num a) => [a] -> Bool
isValid xs = isSortedEitherWay xs && isGradual xs

toleratesOneViolation :: ([a] -> Bool) -> [a] -> Bool
toleratesOneViolation check xs =
    check xs || any (\i -> check (removeAt i xs)) [0..length xs - 1]

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i + 1) xs

isTolerantValid :: (Ord a, Num a) => [a] -> Bool
isTolerantValid xs = toleratesOneViolation isValid xs

main :: IO ()
main = do
    -- part 1
    content <- readFile "2.txt"

    let levels = map parseLine (lines content)

    let results = map isValid levels
    putStrLn $ show $ length $ filter (\x -> x) results 

    -- part 2
    let tolerantResults = map isTolerantValid levels
    putStrLn $ show $ length $ filter (\x -> x) tolerantResults
