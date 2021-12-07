module AOC.Solutions.Day07 (day07) where

import AOC.Solver (Solution, solution)
import Control.Monad (liftM2)
import Data.List (sort)
import Data.Text (Text, split, unpack)
import Prelude hiding (lines)

newtype Day07 = Day07 {crabs :: [Int]}

day07 :: Solution
day07 = solution setup part1 part2

setup :: Text -> Day07
setup = Day07 . map (read . unpack) . split (== ',')

part1 :: Day07 -> Int
part1 (Day07 xs) = let target = median xs in sum $ map (abs . subtract target) xs
  where
    median = liftM2 (!!) sort $ (`div` 2) . length

part2 :: Day07 -> Int
part2 (Day07 ps) = minimumFuelAround (median ps) ps
  where
    median = liftM2 (!!) sort $ (`div` 2) . length

minimumFuelAround :: Int -> [Int] -> Int
minimumFuelAround target ps = minimum $ map (`totalFuelFor` ps) track
  where
    track = around target (0, length ps)

-- TODO: use (n * (n + 1)) / 2
totalFuelFor :: Int -> [Int] -> Int
totalFuelFor target ps = sum $ map (fuelFor target) ps

fuelFor :: Int -> Int -> Int
fuelFor target pos = series !! abs (target - pos)

series :: [Int]
series = map snd $ iterate (\(i, x) -> (i + 1, x + i)) (1, 0)

around :: Int -> (Int, Int) -> [Int]
around x (minB, maxB) = filter (\x -> x >= minB && x <= maxB) $ map (x +) $ concat $ zipWith (\a b -> [a, b]) [0, -1 ..] [1 ..]
