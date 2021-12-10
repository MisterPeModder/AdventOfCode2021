{-# LANGUAGE OverloadedStrings #-}

module AOC.Solutions.Day08 (day08) where

import AOC.Solver (Solution, solution)
import Data.List (nub, sort)
import Data.Text (Text, lines, split, splitOn, unpack)
import Prelude hiding (lines)

newtype Day08 = Day08 [([Signals], [Signals])]

newtype Signals = Signals {fromSignals :: String}

day08 :: Solution
day08 = solution setup part1 part2

setup :: Text -> Day08
setup = Day08 . map parseLine . lines
  where
    parseLine :: Text -> ([Signals], [Signals])
    parseLine input =
      let [uniqueInputs, outputInput] = splitOn " | " input
       in (map (toSignals . unpack) $ splitOn " " uniqueInputs, map (toSignals . unpack) $ splitOn " " outputInput)

part1 :: Day08 -> Int
part1 (Day08 ls) = (\(c1, c4, c7, c8) -> c1 + c4 + c7 + c8) $ foldl countSimple (0, 0, 0, 0) $ concatMap (map deduceSimple . snd) ls
  where
    countSimple :: (Int, Int, Int, Int) -> Maybe Int -> (Int, Int, Int, Int)
    countSimple (c1, c4, c7, c8) (Just 1) = (c1 + 1, c4, c7, c8)
    countSimple (c1, c4, c7, c8) (Just 4) = (c1, c4 + 1, c7, c8)
    countSimple (c1, c4, c7, c8) (Just 7) = (c1, c4, c7 + 1, c8)
    countSimple (c1, c4, c7, c8) (Just 8) = (c1, c4, c7, c8 + 1)
    countSimple c _ = c

part2 :: Day08 -> Int
part2 = error "NYI"

toSignals :: String -> Signals
toSignals = Signals . nub . sort

deduceSimple :: Signals -> Maybe Int
deduceSimple s = case length $ fromSignals s of
  2 -> Just 1
  4 -> Just 4
  3 -> Just 7
  7 -> Just 8
  _ -> Nothing
