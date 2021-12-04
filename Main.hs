module Main where

import AOC.Config (AOCConfig (..), readConfigOrDefault, writeConfig)
import AOC.Error (expect)
import AOC.Input (fetchInput)
import AOC.Solutions.Day03
import AOC.Solver
import Text.Printf (printf)

main :: IO ()
main = do
  config <- readConfigOrDefault
  expect "Failed to write configuration file" $ writeConfig config
  printf "Year %d, Day %d:\n" (year config) (day config)
  input <- expect "Failed to fetch input" $ fetchInput config
  let d3 = setup input :: Day03
  print (part1 d3)
  print (part2 d3)
  putStrLn "done!"
