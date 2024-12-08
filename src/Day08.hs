module Day08 (day08, day08TestInput) where

import Common
import Data.HashMap.Strict qualified as M
import Data.List (nub)

day08TestInput :: String
day08TestInput =
  "............\n\
  \........0...\n\
  \.....0......\n\
  \.......0....\n\
  \....0.......\n\
  \......A.....\n\
  \............\n\
  \............\n\
  \........A...\n\
  \.........A..\n\
  \............\n\
  \............\n\
  \"

day08 :: AOCSolution
day08 input = show <$> (solve <$> [p1, p2] <*> pure m)
  where
    (m, inBounds -> b) = parseInput input
    p1 = part1 b
    p2 = part2 b

parseInput :: String -> (M.HashMap Char [Point2d], (Point2d, Point2d))
parseInput input = (gridMap, bounds $ map snd grid)
  where
    grid =
      [ (v, (i, j))
        | (i, line) <- zip [0 ..] $ lines input,
          (j, v) <- zip [0 ..] line
      ]
    buildMap :: [(Char, Point2d)] -> M.HashMap Char [Point2d]
    buildMap = foldr (\(k, v) -> M.insertWith mappend k [v]) M.empty
    gridMap = buildMap $ filter ((/= '.') . fst) grid

combinations :: [a] -> [(a, a)]
combinations [] = []
combinations (x : xs) = map (x,) xs ++ combinations xs

part1, part2 :: (Point2d -> Bool) -> Point2d -> Point2d -> [Point2d]
part1 inBounds d p = [p' | inBounds p']
  where
    p' = addTuples p d
part2 inBounds d = fn
  where
    fn p = if inBounds p then p : fn (addTuples p d) else []

antinodes :: (Point2d -> Point2d -> [Point2d]) -> (Point2d, Point2d) -> [Point2d]
antinodes fn ((x1, y1), (x2, y2)) =
  fn (-dx, -dy) (x1, y1) ++ fn (dx, dy) (x2, y2)
  where
    dx = x2 - x1
    dy = y2 - y1

solve :: (Point2d -> Point2d -> [Point2d]) -> M.HashMap k [Point2d] -> Int
solve fn =
  length
    . nub
    . concat
    . M.elems
    . M.map (concatMap (antinodes fn) . combinations)

inBounds :: (Point2d, Point2d) -> Point2d -> Bool
inBounds ((a, b), (c, d)) (x, y) = x >= a && x <= c && y >= b && y <= d
