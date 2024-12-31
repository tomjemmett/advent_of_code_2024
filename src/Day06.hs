module Day06 (day06, day06TestInput) where

import Common
import Control.Monad.RWS.Strict
import Control.Parallel.Strategies (parMap, rseq)
import Data.Array qualified as A
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S

type Grid = A.Array Point2d Bool

type Points = [M.HashMap Point2d Direction]

type Visited = S.HashSet (Point2d, Direction)

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
day06 input = show <$> [p1, p2]
  where
    (g, s) = parseInput input
    (a, b) = part1 g s
    p1 = S.size $ S.map fst a
    p2 = sum $ parMap rseq (part2 g) (M.toList $ M.unions b)

part1 :: Grid -> Point2d -> (Visited, Points)
part1 g s = execRWS (go (s, North)) g S.empty
  where
    go (p, d) = do
      tell [M.singleton p d]
      solve go (p, d)

part2 :: Grid -> (Point2d, Direction) -> Int
part2 g (p, d) = fst $ evalRWS (go (p', d')) g' S.empty
  where
    g' = g A.// [(p, False)]
    d' = turn90 d
    p' = moveOneStepInDir p $ turn90 d'
    go = solve go

solve ::
  ((Point2d, Direction) -> RWS Grid Points Visited Int) ->
  (Point2d, Direction) ->
  RWS Grid Points Visited Int
solve go (p, d) = do
  v <- get
  if (p, d) `S.member` v
    then pure 1
    else do
      modify $ S.insert (p, d)
      g <- ask
      let p' = moveOneStepInDir p d
          d' = turn90 d
      case g !?! p' of
        Just True -> go (p', d)
        Just False -> go (p, d')
        Nothing -> pure 0

(!?!) :: (A.Ix i) => A.Array i e -> i -> Maybe e
(!?!) g p =
  if A.inRange (A.bounds g) p
    then Just $ g A.! p
    else Nothing

parseInput :: String -> (Grid, Point2d)
parseInput input = (fmap (/= '#') g, start)
  where
    xs =
      [ ((i, j), v)
        | (i, line) <- zip [0 ..] $ lines input,
          (j, v) <- zip [0 ..] line
      ]
    mxy = maximum $ map fst xs
    g = A.array ((0, 0), mxy) xs
    start = fst $ head $ filter ((== '^') . snd) xs
