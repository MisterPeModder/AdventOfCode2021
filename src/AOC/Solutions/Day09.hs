module AOC.Solutions.Day09 where

import AOC.Solver (Solution, solution)
import Data.Array.IArray (Array (..), listArray)
import Data.Array.Unboxed (UArray (..))
import Data.Text (Text, lines, unpack)
import qualified Data.Text as T
import Data.Word (Word8)
import Prelude hiding (lines)

newtype Day09 = Day09 {getGrid :: Array Word (UArray Word Word8)}

day09 :: Solution
day09 = solution setup part1 part2

setup :: Text -> Day09
setup input = Day09 $ listArray (0, height - 1) $ map parseLine gridLines
  where
    gridLines = lines input
    height = fromIntegral $ length gridLines
    parseLine line = listArray (0, fromIntegral $ T.length line - 1) $ (map (read . return) . unpack) line

part1 :: Day09 -> Int
part1 = error "NYI"

part2 :: Day09 -> Int
part2 = error "NYI"
