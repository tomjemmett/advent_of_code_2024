module Day03 (day03, day03TestInput) where

import Common
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Either (fromRight)
import Text.Parsec ((<|>))
import Text.Parsec qualified as P

day03TestInput :: String
day03TestInput = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

day03 :: AOCSolution
day03 = map show . untuplify2 . parseInput

parseInput :: String -> (Int, Int)
parseInput = fromRight (0, 0) . P.runParser p (True, (0, 0)) ""
  where
    p = P.many (P.choice $ P.try <$> [pDont, pDo, pMul, pNul]) >> snd <$> P.getState

pDoDont :: Bool -> String -> P.Parsec String (Bool, (Int, Int)) ()
pDoDont inc str = P.string str >> P.modifyState (first (const inc))

pDont, pDo, pMul, pNul :: P.Parsec String (Bool, (Int, Int)) ()
pDont = pDoDont False "don't()"
pDo = pDoDont True "do()"
pNul = void P.anyToken
pMul = do
  v <- pMul'
  P.modifyState (\(s, xy) -> (s, addTuples xy (v, fromEnum s * v)))
  where
    pMul' :: P.Parsec String (Bool, (Int, Int)) Int
    pMul' = do
      i <- read <$> (P.string "mul(" *> P.many P.digit)
      j <- read <$> (P.char ',' *> P.many P.digit <* P.char ')')
      pure $ i * j