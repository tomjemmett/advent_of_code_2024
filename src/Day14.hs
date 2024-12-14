module Day14 (day14, day14TestInput) where

import Common
import Data.List (elemIndex, group, sort)
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
day14 input = show <$> [part1, part2]
  where
    i = parseInput input
    wh@(w, h) = if input == day14TestInput then (11, 7) else (101, 103)
    robots = take (w * h) $ iterate (moveRobots wh) i
    safetyScores = map (countQuadrants wh) robots
    part1 = cycle safetyScores !! 100
    -- two assumptions for part 2:
    -- 1. our answer must be in the first 101 * 103 iterations, as the patterns will repeat.
    --    both the height and the width are prime numbers, so any addition will cycle in these fields
    -- 2. the safety score from part 1 might be a hint that our answer will be a minimum value for
    --    the safety score
    Just part2 = elemIndex (minimum safetyScores) safetyScores

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

-- debug function to help check part 2
printGrid :: Point2d -> [Robot] -> IO ()
printGrid (width, height) robots = do
  let rs = map fst robots
  putStrLn $
    unlines $
      [ [ if (x, y) `elem` rs then '#' else ' '
          | x <- [0 .. width - 1]
        ]
        | y <- [0 .. height - 1]
      ]
