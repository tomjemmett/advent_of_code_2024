module Day19 (day19, day19TestInput) where

import Common
import Control.Monad.State
  ( MonadState (get),
    State,
    evalState,
    modify,
  )
import Data.HashMap.Strict qualified as M
import Data.List (isPrefixOf)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day19TestInput :: String
day19TestInput =
  "r, wr, b, g, bwu, rb, gb, br\n\
  \\n\
  \brwrr\n\
  \bggr\n\
  \gbbr\n\
  \rrbgbr\n\
  \ubwu\n\
  \bwurrg\n\
  \brgr\n\
  \bbrgwb\
  \"

day19 :: AOCSolution
day19 = map show . solve . parseInput

parseInput :: String -> ([String], [String])
parseInput = parse do
  towels <- P.many1 P.letter `P.sepBy` P.string ", " <* P.newline
  patterns <- P.newline *> P.many1 P.letter `P.sepBy` P.newline
  pure (towels, patterns)

solve :: ([String], [String]) -> [Int]
solve (towels, patterns) =
  [countTrue (> 0), sum]
    <*> pure (evalState (traverse (go towels) patterns) M.empty)

go :: [String] -> String -> State (M.HashMap String Int) Int
go _ "" = pure 1
go towels p = do
  memo <- get
  case memo M.!? p of
    Just result -> pure result
    Nothing -> do
      let rs = [go towels (drop (length t) p) | t <- towels, t `isPrefixOf` p]
      r <- sum <$> sequence rs
      modify $ M.insert p r
      pure r