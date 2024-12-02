-- Day 1: Historian Hysteria
-- where is big historian man??

-- part 1
-- two lists of numbers, sort each, pair smalles with smallest, for each value, find diff
--  what is total diff?

-- part 2
--  make similarity score by adding up each number in x multiplied by its number of appearences i y
-- 

import System.IO
import Data.List.Split (splitOn)
import Data.List (sort)


parseLine :: String -> (Int, Int)
parseLine line = let [x, y] = map read (splitOn "   " line) in (x, y)

main :: IO ()
main = do
    -- part 1
    content <- readFile "1.txt"

    let pairs = map parseLine (lines content)
        xs = sort $ map fst pairs
        ys = sort $ map snd pairs

    let totalDiff = sum $ zipWith (\x y -> abs(x - y)) xs ys

    putStrLn $ "Diff: " ++ show totalDiff

    -- part 2
    let similarity = sum $ map (\x -> x * ( length $ filter (\y -> x == y) ys)) xs

    putStrLn $ "similarity score: " ++ show similarity