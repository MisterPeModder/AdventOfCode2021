module Main where

import AOC.Config (AOCConfig (..), readConfigOrDefault, writeConfig)
import AOC.Error (expect, expectId, liftResult)
import AOC.Input (fetchInput)
import AOC.Solutions (lookupSolution)
import Text.Printf (printf)

main :: IO ()
main = do
  config <- readConfigOrDefault
  expect "Failed to write configuration file" $ writeConfig config
  input <- expect "Failed to fetch input" $ fetchInput config
  printf "==================\nYear %d, Day %02d:\n==================\n" (year config) (day config)
  solution <- expectId $ liftResult $ lookupSolution (day config)
  let (r1, r2) = solution input
  putStr "- Part 1: "
  print r1
  putStr "- Part 2: "
  print r2
