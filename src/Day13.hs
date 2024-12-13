module Day13 (day13, day13TestInput) where

import Common
import Control.Monad (when)
import Control.Monad.Writer (Writer (..), execWriter, tell)
import Data.List.Split (splitOn)
import Data.Monoid (Sum (..))
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

type Machine = ((Int, Int), (Int, Int), (Int, Int))

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

parseButton :: Parser (Int, Int)
parseButton = P.string "Button " >> P.anyChar >> parseXY

parsePrize :: Parser (Int, Int)
parsePrize = do
  x <- P.string "Prize: X=" *> number
  y <- P.string ", Y=" *> number
  pure (x, y)

parseXY :: Parser (Int, Int)
parseXY = do
  x <- P.string ": X" *> number
  y <- P.string ", Y" *> number
  pure (x, y)

solve :: Int -> [Machine] -> Int
solve i = getSum . execWriter . traverse fn
  where
    fn :: Machine -> Writer (Sum Int) ()
    fn ((x1, y1), (x2, y2), ((+ i) -> xt, (+ i) -> yt)) = do
      let n = x1 * y2 - x2 * y1
          (a, ma) = (xt * y2 - yt * x2) `divMod` n
          (b, mb) = (yt * x1 - xt * y1) `divMod` n
      when (ma == 0 && mb == 0) $ tell (Sum (a * 3 + b))