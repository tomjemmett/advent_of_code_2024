module Day03 (day03, day03TestInput) where

import Common
import Data.Either (fromRight)
import Text.Parsec ((<|>))
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day03TestInput :: String
day03TestInput = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

day03 :: AOCSolution
day03 = map show . untuplify2 . parseInput

parseInput :: String -> (Int, Int)
parseInput = foldr1 addTuples . fromRight [] . P.runParser p True ""
  where
    p = P.many $ P.choice $ P.try <$> [pDont, pDo, pMul, pNul]

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (a, b) (c, d) = (a + c, b + d)

pDont, pDo, pMul, pNul :: P.Parsec String Bool (Int, Int)
pDont = P.string "don't()" >> P.setState False >> pure (0, 0)
pDo = P.string "do()" >> P.setState True >> pure (0, 0)
pNul = P.anyToken >> pure (0, 0)
pMul = do
  v <- pMul'
  inc <- P.getState
  pure (v, v * fromEnum inc)
  where
    pMul' :: P.Parsec String Bool Int
    pMul' = do
      i <- read <$> (P.string "mul(" *> P.many P.digit)
      j <- read <$> (P.char ',' *> P.many P.digit <* P.char ')')
      pure $ i * j
