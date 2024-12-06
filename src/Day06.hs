module Day06 (day06, day06TestInput) where

import Common
import Control.Parallel.Strategies (parMap, rseq)
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (partition)
import Data.Maybe (fromMaybe, isNothing)

day06TestInput :: String
day06TestInput =
  "....#.....\n\
  \.........#\n\
  \..........\n\
  \..#.......\n\
  \.......#..\n\
  \..........\n\
  \.#..^.....\n\
  \........#.\n\
  \#.........\n\
  \......#...\n\
  \"

day06 :: AOCSolution
day06 input = show <$> ([part1, part2] <*> pure (parseInput input))

part1 :: (M.HashMap Point2d Char, Point2d) -> Int
part1 = length . fromMaybe [] . move S.empty North

part2 :: (M.HashMap Point2d Char, Point2d) -> Int
part2 (grid, start) = countTrue isNothing checkGrids
  where
    Just positionsToCheck = move S.empty North (grid, start)
    checkGrids = parMap rseq run positionsToCheck
    run k = move S.empty North (M.insert k '#' grid, start)

move :: S.HashSet (Point2d, Direction) -> Direction -> (M.HashMap Point2d Char, Point2d) -> Maybe [Point2d]
move visited dir (grid, pos)
  | S.member (pos, dir) visited = Nothing
  | otherwise = case M.lookup pos' grid of
      Just '#' -> move visited' (turn90 dir) (grid', pos)
      Just _ -> move visited' dir (grid', pos')
      Nothing -> Just $ M.keys $ M.filter (== 'X') grid'
  where
    pos' = moveOneStepInDir pos dir
    grid' = M.insert pos 'X' grid
    visited' = S.insert (pos, dir) visited

parseInput :: String -> (M.HashMap Point2d Char, Point2d)
parseInput input = (M.insert start '^' $ M.fromList vs, start)
  where
    ((fst -> start) : _, vs) =
      partition
        ((== '^') . snd)
        [ ((i, j), v)
          | (i, line) <- zip [0 ..] $ lines input,
            (j, v) <- zip [0 ..] line
        ]