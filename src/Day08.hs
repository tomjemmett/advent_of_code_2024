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
day08 input = show . length <$> (solve <$> [[1], [0 .. 30]] <*> pure i)
  where
    i = parseInput input

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

antinodes :: (Point2d, Point2d) -> [Int] -> (Point2d, Point2d) -> [Point2d]
antinodes bounds ns ((x1, y1), (x2, y2)) =
  filter
    (inBounds bounds)
    $ concat
      [ [ (x1 - i * dx, y1 - i * dy),
          (x2 + i * dx, y2 + i * dy)
        ]
        | i <- ns
      ]
  where
    dx = x2 - x1
    dy = y2 - y1

solve :: [Int] -> (M.HashMap k [Point2d], (Point2d, Point2d)) -> [Point2d]
solve ns (m, bounds) = nub $ concat $ M.elems m'
  where
    m' = M.map (concatMap (antinodes bounds ns) . combinations) m

inBounds :: (Point2d, Point2d) -> Point2d -> Bool
inBounds ((a, b), (c, d)) (x, y) = x >= a && x <= c && y >= b && y <= d
