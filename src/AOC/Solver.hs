module AOC.Solver (solution, Solution, SolutionPart, SolutionSetup) where

import Control.Monad (liftM2)
import Data.Text (Text)

type SolutionSetup s = (Text -> s)
type SolutionPart s = (s -> Int)
type Solution = Text -> (Int, Int)

solution :: SolutionSetup s -> SolutionPart s -> SolutionPart s -> Solution
solution setup part1 part2 input = liftM2 (,) part1 part2 $ setup input
