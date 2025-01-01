module Day14 (day14, day14TestInput) where

import Common
import Data.Function (on)
import Data.List (group, sort, maximumBy)
import Data.IntMap qualified as IM
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

type Robot = (Point2d, Point2d)

day14TestInput :: String
day14TestInput =
  "p=0,4 v=3,-3\n\
  \p=6,3 v=-1,-3\n\
  \p=10,3 v=-1,2\n\
  \p=2,0 v=2,-1\n\
  \p=0,0 v=1,3\n\
  \p=3,0 v=-2,-2\n\
  \p=7,6 v=-1,-3\n\
  \p=3,0 v=-1,-2\n\
  \p=9,3 v=2,3\n\
  \p=7,3 v=-1,2\n\
  \p=2,4 v=2,-3\n\
  \p=9,5 v=-3,-3\n\
  \"

day14 :: AOCSolution
day14 input = show <$> ([part1, part2] <*> pure wh <*> pure robots)
  where
    i = parseInput input
    wh = if input == day14TestInput then (11, 7) else (101, 103)
    robots = iterate (moveRobots wh) i

part1 :: (Int, Int) -> [[Robot]] -> Int
part1 wh robots = cycle safetyScores !! 100
  where
    safetyScores = map (countQuadrants wh) robots

part2 :: (Int, Int) -> [[Robot]] -> Int
part2 (w, h) robots = r
  where
    (xss, yss) = unzip $ map (unzip . map fst) robots
    getMax = fst . maximumBy (compare `on` snd) . zip [0..] . map fn
    fn = maximum . IM.elems . IM.fromListWith (+) . map (,1)
    x = getMax $ take w xss
    y = getMax $ take h yss
    (r:_) = dropWhile ((/= y) . (`mod` h)) $ iterate (+w) x

parseInput :: String -> [Robot]
parseInput = parse (P.many parseLine)
  where
    parseLine = do
      px <- P.string "p=" *> number
      py <- P.string "," *> number
      vx <- P.string "v=" *> number
      vy <- P.string "," *> number
      pure ((px, py), (vx, vy))

countQuadrants :: Point2d -> [Robot] -> Int
countQuadrants (width, height) = product . drop 1 . map length . group . sort . map f
  where
    f ((x, y), _)
      | x < dx && y < dy = 1
      | x > dx && y < dy = 2
      | x < dx && y > dy = 3
      | x > dx && y > dy = 4
      | otherwise = 0
    dx = width `div` 2
    dy = height `div` 2

moveRobots :: Point2d -> [Robot] -> [Robot]
moveRobots (width, height) = map moveRobot
  where
    moveRobot :: Robot -> Robot
    moveRobot ((px, py), v@(vx, vy)) = (wrapRobot (px + vx, py + vy), v)

    wrapRobot :: Point2d -> Point2d
    wrapRobot (px, py) = (px `mod` width, py `mod` height)

