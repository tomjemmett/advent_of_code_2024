module Day07 (day07, day07TestInput) where

import Common
import Data.List (partition)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day07TestInput :: String
day07TestInput =
  "190: 10 19\n\
  \3267: 81 40 27\n\
  \83: 17 5\n\
  \156: 15 6\n\
  \7290: 6 8 6 15\n\
  \161011: 16 10 13\n\
  \192: 17 8 14\n\
  \21037: 9 7 18 13\n\
  \292: 11 6 16 20\n\
  \"

day07 :: AOCSolution
day07 input = show . sum . map fst <$> [p1, p1 ++ p2]
  where
    i1 = parseInput input
    (p1, i2) = partition (solve False) i1
    (p2, _) = partition (solve True) i2

parseInput :: String -> [(Int, [Int])]
parseInput = map (parse p) . lines
  where
    p = do
      target <- number
      P.string ": "
      nums <- P.many number
      pure (target, nums)

solve :: Bool -> (Int, [Int]) -> Bool
solve isPart2 (target, nums) = fn 0 nums
  where
    fn t _ | t > target = False
    fn t [] = t == target
    fn t (y : ys) = fn (t * y) ys || fn (t + y) ys || (isPart2 && fn (read (show t ++ show y)) ys)