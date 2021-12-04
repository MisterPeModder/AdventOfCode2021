module AOC.Solutions.Day03 (Day03(..)) where

import AOC.Solver
import Data.Bits (Bits (shiftL, (.|.)))
import Data.Text (lines, unpack)
import Prelude hiding (lines)

newtype Day03 = Day03 [[Bool]]

parseBinary :: [Bool] -> Int
parseBinary = foldl addBit 0
  where
    addBit n b = shiftL n 1 .|. (if b then 1 else 0)

instance Solver Day03 where
  setup = Day03 . map (map toBit . unpack) . lines
    where
      toBit c = c == '1'

  part1 (Day03 allBits) = gamma * epsilon
    where
      frequencies = (\(ones, total) -> map (/ total) ones) $ foldl countOnes (repeat 0, 0) allBits
      countOnes (ones, total) bits = (zipWith (+) ones (map (\b -> if b then 1 else 0) bits), total + 1)
      gamma = parseBinary $ map (> 0.5) frequencies
      epsilon = parseBinary $ map (<= 0.5) frequencies

  part2 (Day03 _) = error "WIP"
