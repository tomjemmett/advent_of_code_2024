module Day18 (day18, day18TestInput) where

import Algorithm.Search (aStar, pruning)
import Common
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (inits)

type Memory = S.HashSet Point2d

type Memories = M.HashMap Int Memory

day18TestInput :: String
day18TestInput =
  "5,4\n\
  \4,2\n\
  \4,5\n\
  \3,0\n\
  \2,1\n\
  \6,3\n\
  \2,4\n\
  \1,5\n\
  \0,6\n\
  \3,3\n\
  \2,6\n\
  \5,1\n\
  \1,2\n\
  \5,5\n\
  \2,5\n\
  \6,5\n\
  \1,4\n\
  \0,4\n\
  \6,4\n\
  \1,1\n\
  \6,1\n\
  \1,0\n\
  \0,5\n\
  \1,6\n\
  \2,0\n\
  \"

day18 :: AOCSolution
day18 input = [show p1, p2]
  where
    (memories, size) = parseInput input
    p1T = if size == 6 then 12 else 1024
    Just p1 = findPath size (memories M.! p1T)
    p2 = lines input !! part2 size memories

parseInput :: String -> (Memories, Int)
parseInput input = (M.fromList $ zip [0 ..] is, maximum $ map snd i)
  where
    !i = map (\xs -> read $ ('(' : xs) ++ ")") $ lines input
    !is = map S.fromList $ inits i

findPath :: Int -> Memory -> Maybe Int
findPath size memory = fst <$> aStar next dist (dist end) (== end) (0, 0)
  where
    end = (size, size)
    dist = manhattanDistance
    next :: Point2d -> [Point2d]
    next = point2dNeighbours `pruning` fn
      where
        fn (x, y) = or [x < 0, y < 0, x > size, y > size, (x, y) `S.member` memory]

part2 :: Int -> Memories -> Int
part2 size memories = go 0 n
  where
    n = maximum $ M.keys memories
    go s e
      | s > e = e
      | s == e = pred e
      | otherwise = case findPath size (memories M.! m) of
          Just _ -> go (succ m) e
          Nothing -> go s (pred m)
      where
        m = (e - s + 1) `div` 2 + s