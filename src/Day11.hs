module Day11 (day11, day11TestInput) where

import Common
import Data.HashMap.Strict qualified as M
import Data.List (splitAt)

day11TestInput :: String
day11TestInput = "125 17"

day11 :: AOCSolution
day11 input = show . sum . M.elems . (r !!) <$> [25, 75]
  where
    i = parseInput input
    r = iterate blink i

parseInput :: String -> M.HashMap Int Int
parseInput = M.fromListWith (+) . map ((,1) . read) . words

blink :: M.HashMap Int Int -> M.HashMap Int Int
blink = M.foldrWithKey' f mempty
  where
    f stone count map = foldr (flip (M.insertWith (+)) count) map $ rule stone

rule :: Int -> [Int]
rule stone
  | stone == 0 = [1]
  | even pow = [a, b]
  | otherwise = [stone * 2024]
  where
    pow = length . takeWhile (<= stone) $ iterate (*10) 1
    (a, b) = stone `divMod` (10 ^ (pow `div` 2))
     
