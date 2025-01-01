module Day07 (day07, day07TestInput) where

import Common
import Data.Foldable (foldrM)
import Data.List (partition)
import Data.Maybe (mapMaybe)
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
day07 input = show <$> [a1, a1 + a2]
  where
    i1 = parseInput input
    (p1, i2) = partition (solve [add, mul]) i1
    (p2, _) = partition (solve [add, mul, cat]) i2
    [a1, a2] = sum . map fst <$> [p1, p2]

parseInput :: String -> [(Int, [Int])]
parseInput = map (parse p) . lines
  where
    p = do
      target <- number <* P.string ": "
      nums <- P.many number
      pure (target, nums)

solve :: [Int -> Int -> Maybe Int] -> (Int, [Int]) -> Bool
solve ops (target, x : xs) = x `elem` foldrM fn target xs
  where
    fn a b = mapMaybe (\f -> f a b) ops

add, mul, cat :: Int -> Int -> Maybe Int
add x y = [y - x | y >= x]
mul x y = [y `div` x | y `mod` x == 0]
cat x y = [d | m == x]
  where
    pow = length . takeWhile (<= x) $ iterate (* 10) 1
    (d, m) = y `divMod` (10 ^ pow)
