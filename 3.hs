import System.IO
import Text.Parsec (Parsec, try, anyChar, digit, char, string, (<|>), ParseError, many, parse, optionMaybe )
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import Control.Applicative (optional)
import Data.Maybe (catMaybes)
import Utils (parseLine)

-- Day 3
-- you get some input in the form of a string. The string has instructions, but anything that fits /mul(<Int>,<Int>)/ should match. 
-- Exceptions to this are ignored, such that /mul(<Int>,<Int>]/ would be invalid.
-- parse all correct substrings, do the multiplication and sum the results.

-- Part 2
-- if we parse don't() then any subsequent values should be ignored until we parse do(), which makes us resume parsing mul(x,y)


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

-- TODO: the last line in doParser and dontParser only checks first token for do() or dont() before repeatedly finding mul() or discarding anyChar. 
-- We still want to parse do or Don't, but mulParser and the skip anyChar parser returns a pair of int, while doParser and dontParser returns lists of these

-- figure out how to optionally parse do() or don't() before parsing mul() while making this still dictate the value of a given mul()

-- made doParser and dontParser only return pairs, issue is that they still expect do() or don't() before every other token, which doesn't consider possible multiples of other tokens inbetween mode switching tokens

-- Alternating Parser that handles the parsing based on "do" or "don't" mode
alternatingParser :: Bool -> Parser [(Int, Int)]
alternatingParser isDoMode = do
    next <- optional (try (string "do()") <|> try (string "don't()"))
    case next of
        Just "do()" -> alternatingParser True
        Just "don't()" -> alternatingParser False
        Nothing -> do
            current <- if isDoMode
                       then (try mulParser <|> (anyChar >> pure (0, 0)))
                       else (anyChar >> pure (0, 0))  -- Skip everything in don't mode
            rest <- optional (alternatingParser isDoMode)
            return $ case rest of
                Nothing -> [current]
                Just rs -> current : rs

-- Parser that starts in "don't" mode
parser :: String -> Either ParseError [(Int, Int)]
parser str = parse (alternatingParser False) "" str


productAndSum :: Either ParseError [(Int, Int)] -> Int
productAndSum (Right tuples)    = sum $ map (\(x, y) -> x * y) tuples
productAndSum (Left _)          = 0

main :: IO ()
main = do
    -- Load file content and parse each line with the parser
    content <- readFile "3.txt"

    -- part 1
    print $ productAndSum $ parse multipleMulParser "" content
    -- part 2
    print $ productAndSum $ parse (alternatingParser True) "" content