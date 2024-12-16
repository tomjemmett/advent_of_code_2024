module Day16 (day16, day16TestInput) where

import Common
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (find, nub)
import PathFinding

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
day16 input = show <$> [p1, S.size p2]
  where
    (g, s, e) = parseInput input
    Just bestPath = findPath (fn g) (s, East) (e, East)
    Just p1 = fst <$> find ((== e) . fst . snd) bestPath
    pointsOnPath = takeWhile (/= e) $ dropWhile (== s) $ nub $ map (fst . snd) bestPath
    p2 = S.fromList pointsOnPath <> part2 (g, s, e) p1 pointsOnPath

parseInput :: String -> (M.HashMap Point2d Bool, Point2d, Point2d)
parseInput input = (M.map (/= '#') $ M.fromList i, s, e)
  where
    i = do
      (y, line) <- zip [0 ..] $ lines input
      (x, v) <- zip [0 ..] line
      pure ((y, x), v)
    Just s = fst <$> find ((== 'S') . snd) i
    Just e = fst <$> find ((== 'E') . snd) i

part2 :: (M.HashMap Point2d Bool, Point2d, Point2d) -> Int -> [Point2d] -> S.HashSet Point2d
part2 (g, s, e) best = go
  where
    go [] = S.empty
    go (p : ps) = setPoints <> go ps
      where
        g' = M.insert p False g
        path = findPath (fn g') (s, East) (e, East)
        ans = fst <$> (path >>= find ((== e) . fst . snd))
        Just points = S.fromList . map (fst . snd) <$> path
        setPoints =
          if Just best == ans
            then points
            else S.empty

fn :: M.HashMap Point2d Bool -> (Point2d, Direction) -> [(Int, (Point2d, Direction))]
fn g (p, d) = if g M.! p' then (1, (p', d)) : ds else ds
  where
    p' = moveOneStepInDir p d
    ds = [(1000, (p, i)) | i <- [North, East, South, West], i /= d]