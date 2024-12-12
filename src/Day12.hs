module Day12 (day12, day12TestInput) where

import Common
import Control.Monad (guard)
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (nub, sort)
import Data.Maybe (fromMaybe)

type Grid = M.HashMap Point2d Char

type Region = S.HashSet Point2d

day12TestInput :: String
day12TestInput =
  "RRRRIICCFF\n\
  \RRRRIICCCF\n\
  \VVRRRCCFFF\n\
  \VVRCCCJFFF\n\
  \VVVVCJJCFE\n\
  \VVIVCCJJEE\n\
  \VVIIICJJEE\n\
  \MIIIIIJJEE\n\
  \MIIISIJEEE\n\
  \MMMISSJEEE\n\
  \"

day12 :: AOCSolution
day12 input = show . sum <$> (map <$> [part1, part2] <*> pure r)
  where
    m = parseInput input
    r = createRegions m

parseInput :: String -> Grid
parseInput input = M.fromList do
  (y, l) <- zip [0 ..] $ lines input
  (x, v) <- zip [0 ..] l
  pure ((y, x), v)

part1 :: Region -> Int
part1 r = fn 0 0 rs
  where
    rs = sort $ S.toList r
    fn p a [] = p * a
    fn p a (x : xs) = fn p' (succ a) xs
      where
        ns = filter (`S.member` r) $ point2dNeighbours x
        p' = p + 4 - length ns

part2 :: Region -> Int
part2 r = S.size r * sum (map (length . fn) [North, East, South, West])
  where
    fn d = do
      p <- S.toList r
      let q = moveOneStepInDir p d
          s = moveOneStepInDir p $ turn90 d
          t = moveOneStepInDir s d
      guard $ not (q `S.member` r)
      guard $ not (s `S.member` r) || t `S.member` r
      pure p

createRegions :: Grid -> [Region]
createRegions map = fn map []
  where
    fn m rs =
      if M.null m
        then rs
        else fn m' (r : rs)
      where
        kv = head $ M.toList m
        (m', r) = createRegion kv m

createRegion :: (Point2d, Char) -> Grid -> (Grid, Region)
createRegion (k, v) map = fn map [k] []
  where
    fn m [] rs = (m, S.fromList rs)
    fn m (n : ns) rs = fn m' ns'' (n : rs)
      where
        ns' = filter (\k -> Just v == M.lookup k map) $ point2dNeighbours n
        ns'' = nub $ ns ++ filter (`M.member` m) ns'
        m' = M.delete n m
