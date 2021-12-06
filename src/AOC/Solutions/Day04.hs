{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module AOC.Solutions.Day04 where

import AOC.Solver (Solution, solution)
import Data.List (transpose)
import Data.Text (Text, lines, null, split, unlines, unpack)
import Prelude hiding (lines, null, unlines)

newtype Day04 = Day04 ([Int], [Board])

type Board = [[(Int, Bool)]]

day04 :: Solution
day04 = solution setup part1 part2

setup :: Text -> Day04
setup input = let ~(chosen : boards) = lines input in Day04 (parseChosen chosen, parseBoards boards)
  where
    parseChosen = map (read . unpack) . split (== ',')
    parseBoards = toBoards . map ((,False) . read . unpack) . filter (not . null) . split (\c -> c == ' ' || c == '\n') . unlines
    toBoards = groupsOf 5 <$> groupsOf 25

part1 :: Day04 -> Int
part1 = error "NYI"

part2 :: Day04 -> Int
part2 = error "NYI"

groupsOf :: Int -> [a] -> [[a]]
groupsOf = groupsOf' []
  where
    groupsOf' gss n xs = case splitAt n xs of
      (gs, []) -> gs : gss
      (gs, rest) -> gs : groupsOf' gss n rest

testBoard :: Board -> Bool
testBoard bs = fullLine bs || fullLine (transpose bs)
  where
    fullLine = any (all snd)
