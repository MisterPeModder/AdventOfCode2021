module AOC.Solver (Solver (..)) where

import Data.Text (Text)

class Solver solver where
  setup :: Text -> solver
  part1 :: solver -> Int
  part2 :: solver -> Int
