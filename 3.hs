-- Day 3
-- you get some input in the form of a string. The string have instructions, but anything that fits /mul(<Int>,<Int>)/ should match. 
-- Exceptions to this is ignored, such that /mul(<Int>,<Int>]/ would be invalid
-- parse all correct substrings, do the multiplication and sum the results.

import System.IO
import Text.Parsec (Parsec, try, anyChar, digit, char, string, (<|>), ParseError, many, parse)
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import Control.Applicative (optional)
import Data.Maybe (catMaybes)
import Utils (parseLine)

integer :: Parser Int
integer = read <$> many digit

mulParser :: Parser (Int, Int)
mulParser = do
    _ <- string "mul("
    x <- integer
    _ <- char ','
    y <- integer
    _ <- char ')'
    return (x, y)

safeMulParser :: Parser (Maybe (Int, Int))
safeMulParser = (Just <$> try mulParser) <|> (anyChar *> pure Nothing)

multipleMulParser :: Parser [(Int, Int)]
multipleMulParser = catMaybes <$> many safeMulParser

parser :: String -> Either ParseError [(Int, Int)]
parser str = parse multipleMulParser "" str

productAndSum :: Either ParseError [(Int, Int)] -> Int
productAndSum (Right tuples)    = sum $ map (\(x, y) -> x * y) tuples
productAndSum (Left _)          = 0

main :: IO ()
main = do
    -- part 1
    content <- readFile "3.txt"
    let linesOfFile = lines content
    
    print $ sum $ map productAndSum (map (parse multipleMulParser "") linesOfFile)

