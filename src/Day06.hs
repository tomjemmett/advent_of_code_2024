module Day06 (day06, day06TestInput) where

import Common
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (partition)
import Data.Maybe (fromMaybe, isNothing)

data Direction = North | East | South | West deriving (Show)

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
part1 = fromMaybe 0 . move S.empty North

part2 :: (M.HashMap Point2d Char, Point2d) -> Int
part2 (grid, start) = countTrue isNothing checkGrids
  where
    positionsToCheck = M.keys $ M.filter (== '.') grid
    checkGrids = map run positionsToCheck
    run k = move S.empty North (M.insert k '#' grid, start)

move :: S.HashSet (Point2d, String) -> Direction -> (M.HashMap Point2d Char, Point2d) -> Maybe Int
move visited dir (grid, pos)
  | S.member (pos, show dir) visited = Nothing
  | otherwise = case M.lookup pos' grid of
      Just '#' -> move visited' (turn90 dir) (grid', pos)
      Just _ -> move visited' dir (grid', pos')
      Nothing -> Just $ M.size $ M.filter (== 'X') grid'
  where
    pos' = newPos pos dir
    grid' = M.insert pos 'X' grid
    visited' = S.insert (pos, show dir) visited

newPos :: Point2d -> Direction -> Point2d
newPos (i, j) = \case
  North -> (i - 1, j)
  East -> (i, j + 1)
  South -> (i + 1, j)
  West -> (i, j - 1)

turn90 :: Direction -> Direction
turn90 = \case
  North -> East
  East -> South
  South -> West
  West -> North

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