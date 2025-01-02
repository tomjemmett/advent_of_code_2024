module Day20 (day20, day20TestInput) where

import Common
import Control.Monad.RWS.Strict
import Control.Parallel.Strategies
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.Hashable (Hashable)
import Data.List (find)
import Data.Sequence (Seq ((:<|)), (><))
import Data.Sequence qualified as SQ

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
day20 input = show <$> [p1, sum cheats]
  where
    (g, s) = parseInput input
    !distances = floodFill g s
    cheats@(p1 : _) = parMap rseq (findCheats distances) [2 .. 20]

parseInput :: String -> (Grid, Point2d)
parseInput input = (M.map (/= '#') $ M.fromList i, s)
  where
    i = do
      (y, line) <- zip [0 ..] $ lines input
      (x, v) <- zip [0 ..] line
      pure ((y, x), v)
    Just s = fst <$> find ((== 'S') . snd) i

withinDistance :: Int -> Point2d -> [Point2d]
withinDistance n (x, y) =
  concat
    [ [ (x + i, y - (n - i))
        | i <- [0 .. (n - 1)]
      ],
      [ (x + i, y + (n - i))
        | i <- [n, (n - 1) .. 1]
      ],
      [ (x - i, y + (n - i))
        | i <- [0 .. (n - 1)]
      ],
      [ (x - i, y - (n - i))
        | i <- [n, (n - 1) .. 1]
      ]
    ]

floodFill :: Grid -> Point2d -> M.HashMap Point2d Int
floodFill g s = M.insert s 0 $ snd $ execRWS (go $ SQ.singleton (s, 1)) g (S.singleton s)
  where
    go :: SQ.Seq (Point2d, Int) -> RWS Grid Distances Visited ()
    go SQ.Empty = pure ()
    go ((p, d) :<| ps) = do
      g <- ask
      v <- get
      ps' <- forM (filter (notMember v <&> (g M.!)) $ point2dNeighbours p) $ \n -> do
        modify $ S.insert n
        tell $ M.singleton n $! d
        pure (n, succ d)
      go (ps >< SQ.fromList ps')

notMember :: (Hashable a) => S.HashSet a -> a -> Bool
notMember s = not . (`S.member` s)

findCheats :: M.HashMap Point2d Int -> Int -> Int
findCheats distances d = foldr go 0 $ M.toList distances
  where
    go :: (Point2d, Int) -> Int -> Int
    go (ip, id) acc = acc + (countTrue validCheat $ withinDistance d ip)
      where
        validCheat :: Point2d -> Bool
        validCheat jp = case distances M.!? jp of
          Just jd -> (jd - id - d) >= 100
          Nothing -> False
