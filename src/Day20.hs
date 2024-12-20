module Day20 (day20, day20TestInput) where

import Common
import Control.Monad.RWS
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.Hashable (Hashable)
import Data.List (find, sortBy)

type Grid = M.HashMap Point2d Bool

type Distances = M.HashMap Point2d Int

type Visited = S.HashSet Point2d

day20TestInput :: String
day20TestInput =
  "###############\n\
  \#...#...#.....#\n\
  \#.#.#.#.#.###.#\n\
  \#S#...#.#.#...#\n\
  \#######.#.#.###\n\
  \#######.#.#...#\n\
  \#######.#.###.#\n\
  \###..E#...#...#\n\
  \###.#######.###\n\
  \#...###...#...#\n\
  \#.#####.#.###.#\n\
  \#.#...#.#.#...#\n\
  \#.#.#.#.#.#.###\n\
  \#...#...#...###\n\
  \###############\n\
  \"

day20 :: AOCSolution
day20 input = show . solve <$> [(== 2), (< 21)]
  where
    (g, s) = parseInput input
    distances = floodFill g s
    ds = sortBy (compare `on` snd) $ M.toList distances
    cs = map getCheat $ concat $ combinations ds
    solve f = countTrue (((>= 100) . snd) <&> (f . fst)) cs

parseInput :: String -> (Grid, Point2d)
parseInput input = (M.map (/= '#') $ M.fromList i, s)
  where
    i = do
      (y, line) <- zip [0 ..] $ lines input
      (x, v) <- zip [0 ..] line
      pure ((y, x), v)
    Just s = fst <$> find ((== 'S') . snd) i

floodFill :: Grid -> Point2d -> M.HashMap Point2d Int
floodFill g s = M.insert s 0 $ snd $ execRWS (go [(s, 1)]) g (S.singleton s)
  where
    go :: [(Point2d, Int)] -> RWS Grid Distances Visited ()
    go [] = pure ()
    go ((p, d) : ps) = do
      g <- ask
      v <- get
      ps' <- forM (filter (notMember v <&> (g M.!)) $ point2dNeighbours p) $ \n -> do
        modify $ S.insert n
        tell $ M.singleton n d
        pure (n, succ d)
      go (ps' ++ ps)

notMember :: (Hashable a) => S.HashSet a -> a -> Bool
notMember s = not . (`S.member` s)

getCheat :: ((Point2d, Int), (Point2d, Int)) -> (Int, Int)
getCheat ((ip, id), (jp, jd)) =
  let d = manhattanDistance ip jp
   in (d, jd - id - d)

combinations :: [a] -> [[(a, a)]]
combinations xs | length xs < 2 = []
combinations (x : xs) = map (x,) xs : combinations xs