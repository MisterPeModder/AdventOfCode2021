module AOC.Solutions (lookupSolution, solutions) where

import AOC.Error (Error(..), justOrThrow, Result)
import AOC.Solutions.Day01 (day01)
import AOC.Solutions.Day02 (day02)
import AOC.Solutions.Day03 (day03)
import AOC.Solver (Solution)

solutions :: [(Word, Solution)]
solutions =
  [ (1, day01),
    (2, day02),
    (3, day03)
  ]

lookupSolution :: Word -> Result Solution
lookupSolution day = justOrThrow (Error $ "No solution found for day " ++ show day) $ lookup day solutions