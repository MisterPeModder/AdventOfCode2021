module Solver (Solver (..), Day1 (..), Day2 (..)) where

import Data.Char (isLetter)
import Data.List (partition)
import Data.Text (Text, lines, unpack)
import Data.Word (Word64)
import Prelude hiding (lines)

class Solver solver where
  setup :: Text -> solver
  part1 :: solver -> Int
  part2 :: solver -> Int

newtype Day1 = Day1 [Int]

countIncreasing :: Ord a => [a] -> Int
countIncreasing xs@(_ : ys) = length $ filter (uncurry (<)) $ zip xs ys
countIncreasing _ = error "list must have more than one element"

instance Solver Day1 where
  setup = Day1 . map (read . unpack) . lines
  part1 (Day1 xs) = countIncreasing xs
  part2 (Day1 xs@(_ : ys@(_ : zs))) = countIncreasing $ zipWith3 (((+) .) . (+)) xs ys zs
  part2 _ = error "part 2 list must have more than two elements"

newtype Day2 = Day2 [(Direction, Int)]

data Direction = Forward | Down | Up

matchDirection :: (Int, Int) -> (Direction, Int) -> (Int, Int)
matchDirection (x, y) (d, n) = case d of
  Forward -> (x + n, y)
  Down -> (x, y + n)
  Up -> (x, y - n)

matchDirection2 :: (Int, Int, Int) -> (Direction, Int) -> (Int, Int, Int)
matchDirection2 (x, y, a) (d, n) = case d of
  Forward -> (x + n, y + a * n, a)
  Down -> (x, y, a + n)
  Up -> (x, y, a - n)

instance Solver Day2 where
  setup = Day2 . map (parseLine . unpack) . lines
    where
      parseLine l = let (d, n) = partition isLetter l in (parseDirection d, read $ tail n)
      parseDirection "forward" = Forward
      parseDirection "down" = Down
      parseDirection "up" = Up
      parseDirection _ = error "invalid direction"

  part1 (Day2 xs) = uncurry (*) $ foldl matchDirection (0, 0) xs
  part2 (Day2 xs) = let (x, y, _) = foldl matchDirection2 (0, 0, 0) xs in x * y
