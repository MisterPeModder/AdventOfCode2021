module AOC.Solutions (lookupSolution, solutions) where

import AOC.Error (Error (..), Result, justOrThrow)
import AOC.Solutions.Day01 (day01)
import AOC.Solutions.Day02 (day02)
import AOC.Solutions.Day03 (day03)
import AOC.Solutions.Day04 (day04)
import AOC.Solutions.Day05 (day05)
import AOC.Solutions.Day06 (day06)
import AOC.Solutions.Day07 (day07)
import AOC.Solutions.Day08 (day08)
import AOC.Solutions.Day09 (day09)
import AOC.Solutions.Day10 (day10)
import AOC.Solver (Solution)

solutions :: [(Word, Solution)]
solutions =
  [ (1, day01),
    (2, day02),
    (3, day03),
    (4, day04),
    (5, day05),
    (6, day06),
    (7, day07),
    (8, day08),
    (9, day09),
    (10, day10)
  ]

lookupSolution :: Word -> Result Solution
lookupSolution day = justOrThrow (Error $ "No solution found for day " ++ show day) $ lookup day solutions
