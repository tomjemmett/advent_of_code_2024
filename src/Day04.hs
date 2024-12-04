module Day04 (day04, day04TestInput) where

import Common
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.Maybe (catMaybes)

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
part1 m = S.size $ S.fromList $ concatMap (catMaybes . getStrings) (M.keys m)
  where
    getStrings (i, j) = isXmas <$> [gUp, gDown, gLeft, gRight, gUpLeft, gUpRight, gDownLeft, gDownRight]
      where
        xs = [0 .. 3]
        gUp =
          [(i - x, j) | x <- xs]
        gDown =
          [(i + x, j) | x <- xs]
        gLeft =
          [(i, j - x) | x <- xs]
        gRight =
          [(i, j + x) | x <- xs]
        gUpLeft =
          [(i - x, j - x) | x <- xs]
        gUpRight =
          [(i - x, j + x) | x <- xs]
        gDownLeft =
          [(i + x, j - x) | x <- xs]
        gDownRight =
          [(i + x, j + x) | x <- xs]
        isXmas xs = case map (\k -> M.lookupDefault '.' k m) xs of
          "XMAS" -> Just $ S.fromList xs
          "SAMX" -> Just $ S.fromList xs
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