module Day13 (day13, day13TestInput) where

import Common
import Data.List.Split (splitOn)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

type Machine = ((Integer, Integer), (Integer, Integer), (Integer, Integer))

day13TestInput :: String
day13TestInput =
  "Button A: X+94, Y+34\n\
  \Button B: X+22, Y+67\n\
  \Prize: X=8400, Y=5400\n\
  \\n\
  \Button A: X+26, Y+66\n\
  \Button B: X+67, Y+21\n\
  \Prize: X=12748, Y=12176\n\
  \\n\
  \Button A: X+17, Y+86\n\
  \Button B: X+84, Y+37\n\
  \Prize: X=7870, Y=6450\n\
  \\n\
  \Button A: X+69, Y+23\n\
  \Button B: X+27, Y+71\n\
  \Prize: X=18641, Y=10279\n\
  \"

day13 :: AOCSolution
day13 input = show <$> (solve <$> [0, 10 ^ 13] <*> pure (parseInput input))

parseInput :: [Char] -> [Machine]
parseInput = map parseMachine . splitOn "\n\n"

parseMachine :: String -> Machine
parseMachine = parse do
  a <- parseButton
  b <- parseButton
  p <- parsePrize
  pure (a, b, p)

parseButton :: Parser (Integer, Integer)
parseButton = P.string "Button " >> P.anyChar >> parseXY

parsePrize :: Parser (Integer, Integer)
parsePrize = do
  x <- P.string "Prize: X=" *> number
  y <- P.string ", Y=" *> number
  pure (toInteger x, toInteger y)

parseXY :: Parser (Integer, Integer)
parseXY = do
  x <- P.string ": X" *> number
  y <- P.string ", Y" *> number
  pure (toInteger x, toInteger y)

solve :: Integer -> [Machine] -> Integer
solve i = sum . map fn
  where
    fn :: Machine -> Integer
    fn ((x1, y1), (x2, y2), ((+ i) -> xt, (+ i) -> yt)) =
      if ma == 0 && mb == 0
        then a * 3 + b
        else 0
      where
        n = x1 * y2 - x2 * y1
        (a, ma) = (xt * y2 - yt * x2) `divMod` n
        (b, mb) = (yt * x1 - xt * y1) `divMod` n
