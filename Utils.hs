module Utils ( 
    parseLine
) where

parseLine :: String -> [Int]
parseLine line = map read $ words line