module Day10 (day10, day10TestInput) where

import Common
import Control.Monad (guard)
import Data.Bifunctor (bimap)
import Data.List (nub)
import Data.Maybe (maybeToList)
import Data.Vector qualified as V

day10TestInput :: String
day10TestInput =
  "89010123\n\
  \78121874\n\
  \87430965\n\
  \96549874\n\
  \45678903\n\
  \32019012\n\
  \01329801\n\
  \10456732\n\
  \"

day10 :: AOCSolution
day10 input = show <$> [p1, p2]
  where
    g = parseGrid2d input
    (p1, p2) = solve g

getValue :: Grid2d -> Point2d -> Maybe Int
getValue g (y, x) = g V.!? y >>= (V.!? x)

solve :: Grid2d -> (Int, Int)
solve grid = bimap sum sum $ unzip $ do
  y <- [0 .. V.length grid - 1]
  x <- [0 .. V.length (grid V.! y) - 1]
  pure $ nPathsFromPosition grid (y, x)

nPathsFromPosition :: Grid2d -> Point2d -> (Int, Int)
nPathsFromPosition grid pos =
  if v == 0
    then (length $ nub paths, length paths)
    else (0, 0)
  where
    Just v = getValue grid pos
    paths = walk grid pos 0

walk :: Grid2d -> Point2d -> Int -> [Point2d]
walk grid pos v = do
  n <- point2dNeighbours pos
  nv <- maybeToList $ getValue grid n
  guard $ succ v == nv
  if nv == 9 then pure n else walk grid n nv