module Solver (Solver (..), Day1 (..)) where

import Data.Text (Text, lines, unpack)
import Data.Word (Word64)
import Prelude hiding (lines)

class Solver solver where
  setup :: Text -> solver
  part1 :: solver -> Int
  part2 :: solver -> Int

newtype Day1 = Day1 [Int]

countIncreasing :: Ord a => [a] -> Int
countIncreasing xs@(_:ys) = length $ filter (uncurry (<)) $ zip xs ys

instance Solver Day1 where
  setup = Day1 . map (read . unpack) . lines
  part1 (Day1 xs) = countIncreasing xs
  part2 (Day1 xs@(_:ys@(_:zs))) = countIncreasing $ zipWith3 (((+) .) . (+)) xs ys zs
