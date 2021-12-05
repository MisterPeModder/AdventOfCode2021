module AOC.Solutions.Day03 (day03) where

import AOC.Solver (Solution, solution)
import Control.Monad (liftM2)
import Data.Bifunctor (Bifunctor (first))
import Data.Bits (Bits (shiftL, (.|.)))
import Data.Text (Text, lines, unpack)
import Prelude hiding (lines)

newtype Day03 = Day03 [NumBits]

type NumBits = [Bool]

type Criteria = [Double] -> NumBits

day03 :: Solution
day03 = solution setup part1 part2

setup :: Text -> Day03
setup = Day03 . map (map toBit . unpack) . lines
  where
    toBit c = c == '1'

part1 :: Day03 -> Int
part1 (Day03 bss) = fromIntegral $ liftM2 (*) (fromBits . gamma) (fromBits . epsilon) $ bitFreq bss

part2 :: Day03 -> Int
part2 (Day03 bss) = fromIntegral $ oxygenRating * scrubberRating
  where
    oxygenRating = getRating gamma bss
    scrubberRating = getRating epsilon bss

getRating :: Criteria -> [NumBits] -> Word
getRating bitCriteria bss = fromBits . snd . head . snd . until isSingleton select $ (bitCriteria, zip bss bss)
  where
    isSingleton (_, [_]) = True
    isSingleton _ = False
    criteriaHead criteria candidates = head $ criteria $ bitFreq (map fst candidates)
    select (criteria, candidates) =
      let c = criteriaHead criteria candidates
       in (criteria, map (first tail) $ filter (\(bs, _) -> head bs == c) candidates)

-- |
-- Converts a list of bits to a number.
fromBits :: NumBits -> Word
fromBits = foldl addBit 0
  where
    addBit n b = shiftL n 1 .|. fromIntegral (fromEnum b)

-- |
-- Returns a list of frequencies of the bit 1 for each position in the given list of binary numbers.
bitFreq :: [NumBits] -> [Double]
bitFreq bs = map ((/ total) . fromIntegral) $ sumBits bs
  where
    total = fromIntegral $ length bs
    sumBits = foldl (zipWith $ (. fromEnum) . (+)) (repeat 0)

-- |
-- The gamma criteria: oxygen generator rating.
gamma :: Criteria
gamma = map (>= 0.5)

-- |
-- The epsilon criteria: CO2 scrubber rating.
epsilon :: Criteria
epsilon = map (< 0.5)
