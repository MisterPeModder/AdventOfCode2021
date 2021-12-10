module AOC.Solutions.Day10 (day10) where

import AOC.Solver (Solution, solution)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text, lines, unpack)
import Prelude hiding (lines)

newtype Day10 = Day10 {inputLines :: [String]}

type ToComplete = [Char]

day10 :: Solution
day10 = solution setup part1 part2

setup :: Text -> Day10
setup = Day10 . map unpack . lines

part1 :: Day10 -> Int
part1 = sum . map (maybe 0 (pointsFor . fst) . findIllegal []) . inputLines
  where
    pointsFor ')' = 3
    pointsFor ']' = 57
    pointsFor '}' = 1197
    pointsFor '>' = 25137
    pointsFor _ = 0

part2 :: Day10 -> Int
part2 = getMiddle . sort . map (calcScore 0) . filter (not . null) . mapMaybe (fmap snd . findIllegal []) . inputLines
  where
    getMiddle xs = length xs `div` 2

findIllegal :: ToComplete -> String -> Maybe (Char, ToComplete)
findIllegal ('(' : stack) (')' : line) = findIllegal stack line
findIllegal ('[' : stack) (']' : line) = findIllegal stack line
findIllegal ('{' : stack) ('}' : line) = findIllegal stack line
findIllegal ('<' : stack) ('>' : line) = findIllegal stack line
findIllegal stack ('(' : line) = findIllegal ('(' : stack) line
findIllegal stack ('[' : line) = findIllegal ('[' : stack) line
findIllegal stack ('{' : line) = findIllegal ('{' : stack) line
findIllegal stack ('<' : line) = findIllegal ('<' : stack) line
findIllegal [] "" = Nothing
-- illegal line
findIllegal _ (c : _) = Just (c, [])
-- incomplete line
findIllegal stack _ = Just ('?', stack)

calcScore :: Int -> ToComplete -> Int
calcScore s [] = s
calcScore s (x : xs) = calcScore (s * 5 + baseScore x) xs
  where
    baseScore '(' = 1
    baseScore '[' = 2
    baseScore '{' = 3
    baseScore '<' = 4
    baseScore c = error $ "Incorrect opening character " ++ show c
