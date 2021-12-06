module AOC.Solutions.Day06 (day06) where

import AOC.Solver (Solution, solution)
import Data.Text (Text, split, unpack)

newtype Day06 = Day06 { initialFishes :: [Int] }

type FishCountPerDay = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

day06 :: Solution
day06 = solution setup part1 part2

setup :: Text -> Day06
setup = Day06 . map (read . unpack) . split (== ',')

part1 :: Day06 -> Int
part1 = countAtDay 80 . initialFishes

part2 :: Day06 -> Int
part2 = countAtDay 256 . initialFishes

countAtDay :: Int -> [Int] -> Int
countAtDay day = sum . map (fcount . (!! day) . iterate fcycle . initDayCount)

initDayCount :: Int -> FishCountPerDay
initDayCount 0 = (1, 0, 0, 0, 0, 0, 0, 0, 0)
initDayCount 1 = (0, 1, 0, 0, 0, 0, 0, 0, 0)
initDayCount 2 = (0, 0, 1, 0, 0, 0, 0, 0, 0)
initDayCount 3 = (0, 0, 0, 1, 0, 0, 0, 0, 0)
initDayCount 4 = (0, 0, 0, 0, 1, 0, 0, 0, 0)
initDayCount 5 = (0, 0, 0, 0, 0, 1, 0, 0, 0)
initDayCount 6 = (0, 0, 0, 0, 0, 0, 1, 0, 0)
initDayCount 7 = (0, 0, 0, 0, 0, 0, 0, 1, 0)
initDayCount 8 = (0, 0, 0, 0, 0, 0, 0, 0, 1)
initDayCount x = error $ "day count cannot exceed 8, got " ++ show x

fcycle :: FishCountPerDay -> FishCountPerDay
fcycle (c0, c1, c2, c3, c4, c5, c6, c7, c8) = (c1, c2, c3, c4, c5, c6, c7 + c0, c8, c0)

fcount :: FishCountPerDay -> Int
fcount (c0, c1, c2, c3, c4, c5, c6, c7, c8) = c0 + c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8
