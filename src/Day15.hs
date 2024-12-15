module Day15 (day15, day15TestInput) where

import Common
import Data.HashMap.Strict qualified as M
import Data.List.Split (splitOn)

day15TestInput :: String
day15TestInput =
  "##########\n\
  \#..O..O.O#\n\
  \#......O.#\n\
  \#.OO..O.O#\n\
  \#..O@..O.#\n\
  \#O#..O...#\n\
  \#O..O..O.#\n\
  \#.OO.O.OO#\n\
  \#....O...#\n\
  \##########\n\
  \\n\
  \<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\n\
  \vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n\
  \><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n\
  \<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n\
  \^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n\
  \^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n\
  \>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n\
  \<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n\
  \^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\n\
  \v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^\
  \"

day15 :: AOCSolution
day15 input = show . solve <$> [p1, p2]
  where
    (m, r) = parseInput input
    p1 = ((m, findRobot m), r)
    m' = doubleMap m
    p2 = ((m', findRobot m'), r)

step :: (M.HashMap Point2d Char, Point2d) -> Char -> (M.HashMap Point2d Char, Point2d)
step (m, s) i = case canMove m d [(s, '.')] of
  Just m' -> (m', addTuples s d)
  Nothing -> (m, s)
  where
    m' = canMove m d [(s, '.')]
    d = case i of
      '>' -> (0, 1)
      '<' -> (0, -1)
      '^' -> (-1, 0)
      'v' -> (1, 0)

canMove :: M.HashMap Point2d Char -> Point2d -> [(Point2d, Char)] -> Maybe (M.HashMap Point2d Char)
canMove m _ [] = Just m
canMove m d@(i, _) ((p, u) : ps) = case v of
  '.' -> canMove m' d ps
  '#' -> Nothing
  '[' -> canMove m' d $ if i == 0 || u == '.' then ps' else (addTuples p (0, 1), '.') : ps'
  ']' -> canMove m' d $ if i == 0 || u == '.' then ps' else (addTuples p (0, -1), '.') : ps'
  _ -> canMove m' d ps'
  where
    v = m M.! p
    m' = M.insert p u m
    p' = addTuples p d
    ps' = (p', v) : ps

solve :: ((M.HashMap Point2d Char, Point2d), String) -> Int
solve = sum . map score . M.keys . M.filter (`elem` "O[") . fst . uncurry (foldl step)
  where
    score (y, x) = y * 100 + x

parseInput :: String -> (M.HashMap Point2d Char, String)
parseInput input = (parseMap m, concat $ lines r)
  where
    [m, r] = splitOn "\n\n" input

parseMap :: String -> M.HashMap Point2d Char
parseMap input = M.fromList m
  where
    m = do
      (y, line) <- zip [0 ..] $ lines input
      (x, v) <- zip [0 ..] line
      pure ((y, x), v)

findRobot :: M.HashMap Point2d Char -> Point2d
findRobot = head . M.keys . M.filter (== '@')

doubleMap :: M.HashMap Point2d Char -> M.HashMap Point2d Char
doubleMap map = M.fromList $ concatMap double $ M.toList map
  where
    double ((y, x), v) = [((y, 2 * x), v1), ((y, 2 * x + 1), v2)]
      where
        v1 = if v == 'O' then '[' else v
        v2 = case v of
          'O' -> ']'
          '@' -> '.'
          _ -> v
