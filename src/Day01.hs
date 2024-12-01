module Day01 (day01, day01TestInput) where

import Common
import Data.HashMap.Strict qualified as M
import Data.List (group, sort, transpose)

day01TestInput :: String
day01TestInput =
  "3   4\n\
  \4   3\n\
  \2   5\n\
  \1   3\n\
  \3   9\n\
  \3   3\n\
  \"

day01 :: AOCSolution
day01 input = map show $ [part1, part2] <*> parseInput input

parseInput :: (Applicative f) => String -> f [[Int]]
parseInput = pure . map2 read . map sort . transpose . map words . lines

part1 :: [[Int]] -> Int
part1 = sum . map abs . uncurry (zipWith (-)) . tuplify2

part2 :: [[Int]] -> Int
part2 [i, j] = sum $ map f i
  where
    j' = M.fromList $ map (\x -> (head x, length x)) $ group j
    f v = v * M.lookupDefault 0 v j'