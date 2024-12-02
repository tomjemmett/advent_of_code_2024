module Day02 where

import Common

day02TestInput :: String
day02TestInput =
  "7 6 4 2 1\n\
  \1 2 7 8 9\n\
  \9 7 6 2 1\n\
  \1 3 2 4 5\n\
  \8 6 4 4 1\n\
  \1 3 6 7 9"

day02 :: AOCSolution
day02 input = show <$> (countTrue <$> [part1, part2] <*> parseInput input)

parseInput :: (Applicative f) => String -> f [[Int]]
parseInput = pure . map (map read . words) . lines

part1 :: [Int] -> Bool
part1 xs = check ((<= 3) . abs) && (check (> 0) || check (< 0))
  where
    check f = all f $ zipWith (-) xs (tail xs)

part2 :: [Int] -> Bool
part2 xs = part1 xs || p2 [] xs
  where
    p2 _ [] = False
    p2 ys (x : xs) = part1 (ys ++ xs) || p2 (ys ++ [x]) xs
