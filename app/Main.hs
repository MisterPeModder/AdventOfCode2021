module Main where

import AOCConfig (AOCConfig (..), readConfigOrDefault, writeConfig)
import qualified Data.Text as T
import Error (expect)
import Input (fetchInput)
import Solver
import Text.Printf (printf)

main :: IO ()
main = do
  config <- readConfigOrDefault
  expect "Failed to write configuration file" $ writeConfig config
  printf "Year %d, Day %d:\n" (year config) (day config)
  input <- expect "Failed to fetch input" $ fetchInput config
  let d1 = setup input :: Day1
  print (part1 d1)
  print (part2 d1)
  putStrLn "done!"
