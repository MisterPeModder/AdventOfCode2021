module Solver (Solver (..), Day1 (..)) where

import Data.Text (Text, lines, unpack)
import Data.Word (Word64)
import Prelude hiding (lines)

class Solver solver where
  setup :: Text -> solver
  part1 :: solver -> Word64
  part2 :: solver -> Word64

newtype Day1 = Day1 [Word64]

instance Solver Day1 where
  setup = Day1 . map (read . unpack) . lines
  part1 (Day1 xs) = head $ [x * y | x <- xs, y <- xs, x + y == 2020]
  part2 (Day1 xs) = head $ [x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]
