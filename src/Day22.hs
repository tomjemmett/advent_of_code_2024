module Day22 (day22, day22TestInput) where

import Common
import Control.Parallel.Strategies (parMap, rseq)
import Data.Bits (Bits (xor))
import Data.HashMap.Strict qualified as M

day22TestInput :: String
day22TestInput =
  "1\n\
  \10\n\
  \100\n\
  \2024\n\
  \"

day22 :: AOCSolution
day22 input = show <$> ([part1, part2] <*> pure evolutions)
  where
    i = parseInput input
    !evolutions = parMap rseq (take 2000 . iterate evolve) i

parseInput :: String -> [Int]
parseInput = map read . lines

part1 :: [[Int]] -> Int
part1 = sum . (!! 2000)

part2 :: [[Int]] -> Int
part2 = maximum . M.elems . foldr1 (M.unionWith (+)) . seqs
  where
    seqs = parMap rseq (build . getSequences)
    build :: [([Int], Int)] -> M.HashMap [Int] Int
    build = foldr (uncurry M.insert) M.empty
    getSequences :: [Int] -> [([Int], Int)]
    getSequences xs = zip (f d) (drop 4 xs')
      where
        xs' = map (`mod` 10) xs
        d = zipWith (-) (drop 1 xs') xs'
        f :: [Int] -> [[Int]]
        f xs
          | length xs < 4 = []
          | otherwise = take 4 xs : f (drop 1 xs)

evolve :: Int -> Int
evolve i = c
  where
    a = prune $ i `xor` (i * 64)
    b = prune $ a `xor` (a `div` 32)
    c = prune $ b `xor` (b * 2048)
    prune = flip mod 16777216
