module AOC.Solutions.Day10 (day10) where

import AOC.Solver (Solution, solution)
import Data.List (sort)
import Data.Text (Text, lines, unpack)
import Debug.Trace (trace)
import Prelude hiding (lines)

newtype Day10 = Day10 {inputLines :: [String]}

type ToComplete = [Char]

day10 :: Solution
day10 = solution setup part1 part2

setup :: Text -> Day10
setup = Day10 . map unpack . lines

part1 :: Day10 -> Int
part1 = sum . map (maybe 0 (pointsFor . fst) . findIllegal []) . inputLines

toComplete :: Day10 -> [ToComplete]
toComplete = foldl addJust [] . map (fmap snd . findIllegal []) . inputLines
  where
    addJust xs (Just x) = x : xs
    addJust xs _ = xs

calcScore :: Int -> ToComplete -> Int
calcScore s ('(' : xs) = calcScore (s * 5 + 1) xs
calcScore s ('[' : xs) = calcScore (s * 5 + 2) xs
calcScore s ('{' : xs) = calcScore (s * 5 + 3) xs
calcScore s ('<' : xs) = calcScore (s * 5 + 4) xs
calcScore s [] = s
calcScore s xs = trace ("ZERO: s=" ++ show s ++ ", xs=" ++ show xs) 0

part2 :: Day10 -> Int
part2 = getMiddle . sort . map (calcScore 0) . filter (not . null) . toComplete
  where
    getMiddle xs = trace ("GET_MIDDLE: xs=" ++ show xs) xs !! (length xs `div` 2)

pointsFor :: Char -> Int
pointsFor ')' = 3
pointsFor ']' = 57
pointsFor '}' = 1197
pointsFor '>' = 25137
pointsFor _ = 0

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
