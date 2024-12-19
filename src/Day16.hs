module Day16 (day16, day16TestInput) where

import Algorithm.Search (aStar, pruning)
import Common
import Control.Parallel.Strategies (parMap, rseq)
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (find, nub)
import Data.Maybe (mapMaybe)

day16TestInput :: String
day16TestInput =
  "#################\n\
  \#...#...#...#..E#\n\
  \#.#.#.#.#.#.#.#.#\n\
  \#.#.#.#...#...#.#\n\
  \#.#.#.#.###.#.#.#\n\
  \#...#.#.#.....#.#\n\
  \#.#.#.#.#.#####.#\n\
  \#.#...#.#.#.....#\n\
  \#.#.#####.#.###.#\n\
  \#.#.#.......#...#\n\
  \#.#.###.#####.###\n\
  \#.#.#...#.....#.#\n\
  \#.#.#.#####.###.#\n\
  \#.#.#.........#.#\n\
  \#.#.#.#########.#\n\
  \#S#.............#\n\
  \#################\n\
  \"

day16 :: AOCSolution
day16 input = show <$> [p1, p2]
  where
    i = parseInput input
    Just (p1, pts) = getPath East i
    p2 = part2 i (p1, pts)

getPath :: Direction -> (M.HashMap Point2d Bool, Point2d, Point2d) -> Maybe (Int, [(Point2d, Direction)])
getPath dir (g, s, e) = aStar nextStates cost heur goal (s, dir)
  where
    goal (p, _) = p == e
    heur (p, _) = manhattanDistance p e
    cost (_, d1) (_, d2) = if d1 == d2 then 1 else 1000
    nextStates (p, d) =
      let p' = moveOneStepInDir p d
          ds = [(p, i) | i <- [North, East, South, West], i /= d]
       in if g M.! p' then (p', d) : ds else ds

part2 :: (M.HashMap Point2d Bool, Point2d, Point2d) -> (Int, [(Point2d, Direction)]) -> Int
part2 (g, s, e) = S.size . S.union (S.fromList [s, e]) . getAllPoints
  where
    getAllPoints :: (Int, [(Point2d, Direction)]) -> S.HashSet Point2d
    getAllPoints (t, ps) = S.fromList (map fst ps) <> S.unions others
      where
        junctions = findJunctions g ps
        others = parMap rseq getAllPoints $ mapMaybe (tryNew (g, s, e) t) junctions

tryNew ::
  (M.HashMap Point2d Bool, Point2d, Point2d) ->
  Int ->
  (Point2d, Point2d, Direction) ->
  Maybe (Int, [(Point2d, Direction)])
tryNew (g, s, e) b (p, r, d) = case v of
  Just (l2, p2) ->
    if b == l1 + l2
      then Just (l2, p2)
      else Nothing
  _ -> Nothing
  where
    Just (l1, p1) = getPath East (g, s, p)
    v = getPath d (M.insert r False g, p, e)

findJunctions ::
  M.HashMap Point2d Bool ->
  [(Point2d, Direction)] ->
  [(Point2d, Point2d, Direction)]
findJunctions g ps | length ps < 2 = []
findJunctions g ((p1, d1) : p@(p2, d2) : ps)
  | ns <= 2 = nxt
  | d1 `elem` [North, South] && d2 == d1 = same
  | d1 `elem` [East, West] && d2 == d1 = same
  | otherwise = (p1, moveOneStepInDir p2 d2, d1) : drop 1 nxt
  where
    ns = countTrue (g M.!) $ point2dNeighbours p1
    nxt = findJunctions g (p : ps)
    same = (p1, p2, d1) : nxt

parseInput :: String -> (M.HashMap Point2d Bool, Point2d, Point2d)
parseInput input = (M.map (/= '#') $ M.fromList i, s, e)
  where
    i = do
      (y, line) <- zip [0 ..] $ lines input
      (x, v) <- zip [0 ..] line
      pure ((y, x), v)
    Just s = fst <$> find ((== 'S') . snd) i
    Just e = fst <$> find ((== 'E') . snd) i
