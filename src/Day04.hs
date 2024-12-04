module Day04 (day04, day04TestInput) where

import Common
import Data.HashMap.Strict qualified as M
import Data.Maybe (catMaybes, mapMaybe)

day04TestInput :: [String]
day04TestInput = [day04TestInputPart1, day04TestInputPart2]

day04TestInputPart1 :: String
day04TestInputPart1 =
  "....XXMAS.\n\
  \.SAMXMS...\n\
  \...S..A...\n\
  \..A.A.MS.X\n\
  \XMASAMX.MM\n\
  \X.....XA.A\n\
  \S.S.S.S.SS\n\
  \.A.A.A.A.A\n\
  \..M.M.M.MM\n\
  \.X.X.XMASX\
  \"

day04TestInputPart2 :: String
day04TestInputPart2 =
  ".M.S......\n\
  \..A..MSMS.\n\
  \.M.S.MAA..\n\
  \..A.ASMSM.\n\
  \.M.S.M....\n\
  \..........\n\
  \S.S.S.S.S.\n\
  \.A.A.A.A..\n\
  \M.M.M.M.M.\n\
  \..........\n\
  \"

day04 :: AOCSolution
day04 input = show <$> [part1 i, part2 i]
  where
    i = parseInput input

part1 :: M.HashMap Point2d Char -> Int
part1 m = sum $ map checks xs
  where
    xs = M.keys $ M.filter (== 'X') m
    -- perform the checks in the 8 directions
    checks :: Point2d -> Int
    checks p =
      length . catMaybes $ [check i j p | i <- [-1 .. 1], j <- [-1 .. 1], (i, j) /= (0, 0)]
    -- perform the check by looking at the next 3 points. If the values aren't M, A, S in sequence
    -- then we will get Nothing returned. Otherwise, return Just ()
    check :: Int -> Int -> Point2d -> Maybe ()
    check i j p@(x, y) =
      pointIsChar p1 'M'
        >> pointIsChar p2 'A'
        >> pointIsChar p3 'S'
        >> Just ()
      where
        [p1, p2, p3] = [(x + i * n, y + j * n) | n <- [1 .. 3]]
    -- helper function to check if a point is the correct character
    -- returns Just () if it is, or Nothing if it isn't (or the point is out of bounds)
    pointIsChar :: Point2d -> Char -> Maybe ()
    pointIsChar p v = case (== v) <$> M.lookup p m of
      Just True -> Just ()
      _ -> Nothing

part2 :: M.HashMap Point2d Char -> Int
part2 m = countTrue isXmas $ M.keys m
  where
    isXmas (i, j) = lDiag && rDiag
      where
        lDiag = getStr [(i + x, j + x) | x <- [-1 .. 1]]
        rDiag = getStr [(i - x, j + x) | x <- [-1 .. 1]]
        getStr xs = case map (\k -> M.lookupDefault '.' k m) xs of
          "MAS" -> True
          "SAM" -> True
          _ -> False

parseInput :: String -> M.HashMap Point2d Char
parseInput input = M.fromList do
  (i, line) <- zip [0 ..] $ lines input
  (j, v) <- zip [0 ..] line
  pure ((i, j), v)