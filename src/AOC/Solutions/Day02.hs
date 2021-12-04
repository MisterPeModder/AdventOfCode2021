module AOC.Solutions.Day02 (Day02(..)) where

import AOC.Solver
import Data.Char (isLetter)
import Data.List (partition)
import Data.Text (lines, unpack)
import Prelude hiding (lines)

newtype Day02 = Day02 [(Direction, Int)]

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

instance Solver Day02 where
  setup = Day02 . map (parseLine . unpack) . lines
    where
      parseLine l = let (d, n) = partition isLetter l in (parseDirection d, read $ tail n)
      parseDirection "forward" = Forward
      parseDirection "down" = Down
      parseDirection "up" = Up
      parseDirection _ = error "invalid direction"

  part1 (Day02 xs) = uncurry (*) $ foldl matchDirection (0, 0) xs
  part2 (Day02 xs) = let (x, y, _) = foldl matchDirection2 (0, 0, 0) xs in x * y
