module AOC.Solutions.Day06 where

import AOC.Solver (Solution, solution)
import Data.Text (Text, lines, split, unpack)
import Debug.Trace (trace)
import Prelude hiding (lines)

newtype Day06 = Day06 [Int]

day06 :: Solution
day06 = solution setup part1 part2

setup :: Text -> Day06
setup = Day06 . map (read . unpack) . split (== ',')

part1 :: Day06 -> Int
part1 (Day06 xs) = length $ iterate fishCycle xs !! 80

part2 :: Day06 -> Int
part2 (Day06 xs) = length $ iterate fishCycle xs !! 256

fishCycle :: [Int] -> [Int]
fishCycle = concatMap fish
  where
    fish 0 = [6, 8]
    fish x = [x - 1]
