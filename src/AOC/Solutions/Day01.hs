module AOC.Solutions.Day01 (day01) where

import AOC.Solver (Solution, solution)
import Data.Text (Text, lines, unpack)
import Prelude hiding (lines)

newtype Day01 = Day01 [Int]

day01 :: Solution
day01 = solution setup part1 part2

setup :: Text -> Day01
setup = Day01 . map (read . unpack) . lines

part1 :: Day01 -> Int
part1 (Day01 xs) = countIncreasing xs

part2 :: Day01 -> Int
part2 (Day01 xs@(_ : ys@(_ : zs))) = countIncreasing $ zipWith3 (((+) .) . (+)) xs ys zs
part2 _ = error "part 2 list must have more than two elements"

countIncreasing :: Ord a => [a] -> Int
countIncreasing xs@(_ : ys) = length $ filter (uncurry (<)) $ zip xs ys
countIncreasing _ = error "list must have more than one element"
