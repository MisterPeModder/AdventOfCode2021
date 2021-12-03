module Solver (Solver (..), Day1 (..), Day2 (..), Day3 (..)) where

import Data.Bits (Bits (shiftL, (.|.)), shift)
import Data.Char (isLetter)
import Data.List (partition)
import Data.Text (Text, lines, unpack)
import Data.Word (Word64)
import Debug.Trace (trace, traceId)
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

newtype Day3 = Day3 [[Bool]]

parseBinary :: [Bool] -> Int
parseBinary = foldl addBit 0
  where
    addBit n b = shiftL n 1 .|. (if b then 1 else 0)

instance Solver Day3 where
  setup = Day3 . map (map toBit . unpack) . lines
    where
      toBit c = c == '1'

  part1 (Day3 allBits) = gamma * epsilon
    where
      frequencies = (\(ones, total) -> map (/ total) ones) $ foldl countOnes (repeat 0, 0) allBits
      countOnes (ones, total) bits = (zipWith (+) ones (map (\b -> if b then 1 else 0) bits), total + 1)
      gamma = parseBinary $ map (> 0.5) frequencies
      epsilon = parseBinary $ map (<= 0.5) frequencies

  part2 (Day3 _) = error "WIP"
