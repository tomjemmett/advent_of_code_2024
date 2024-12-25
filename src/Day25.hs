module Day25 (day25, day25TestInput) where

import Common
import Data.Either (partitionEithers)
import Data.List (transpose)
import Data.List.Split (splitOn)

day25TestInput :: String
day25TestInput =
  "#####\n\
  \.####\n\
  \.####\n\
  \.####\n\
  \.#.#.\n\
  \.#...\n\
  \.....\n\
  \\n\
  \#####\n\
  \##.##\n\
  \.#.##\n\
  \...##\n\
  \...#.\n\
  \...#.\n\
  \.....\n\
  \\n\
  \.....\n\
  \#....\n\
  \#....\n\
  \#...#\n\
  \#.#.#\n\
  \#.###\n\
  \#####\n\
  \\n\
  \.....\n\
  \.....\n\
  \#.#..\n\
  \###..\n\
  \###.#\n\
  \###.#\n\
  \#####\n\
  \\n\
  \.....\n\
  \.....\n\
  \.....\n\
  \#....\n\
  \#.#..\n\
  \#.#.#\n\
  \#####\n\
  \"

day25 :: AOCSolution
day25 input = [show $ solve $ parseInput input]

parseInput :: String -> ([[Int]], [[Int]])
parseInput = partitionEithers . map (parseKeyOrLock . lines) . splitOn "\n\n"

parseKeyOrLock :: [String] -> Either [Int] [Int]
parseKeyOrLock xs =
  if head xs == "#####"
    then Left v
    else Right v
  where
    v = map (pred . countTrue (== '#')) $ transpose xs

solve :: ([[Int]], [[Int]]) -> Int
solve ([], _) = 0
solve (l : ls, ks) = countTrue (all (<= 5) . zipWith (+) l) ks + solve (ls, ks)