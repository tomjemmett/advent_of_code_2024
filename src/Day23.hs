module Day23 (day23, day23TestInput) where

import Common
import Control.Monad (guard)
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (intercalate, sort, tails)
import Text.Parsec qualified as P

day23TestInput :: String
day23TestInput =
  "kh-tc\n\
  \qp-kh\n\
  \de-cg\n\
  \ka-co\n\
  \yn-aq\n\
  \qp-ub\n\
  \cg-tb\n\
  \vc-aq\n\
  \tb-ka\n\
  \wh-tc\n\
  \yn-cg\n\
  \kh-ub\n\
  \ta-co\n\
  \de-co\n\
  \tc-td\n\
  \tb-wq\n\
  \wh-td\n\
  \ta-ka\n\
  \td-qp\n\
  \aq-cg\n\
  \wq-ub\n\
  \ub-vc\n\
  \de-ta\n\
  \wq-aq\n\
  \wq-vc\n\
  \wh-yn\n\
  \ka-de\n\
  \kh-ta\n\
  \co-tc\n\
  \wh-qp\n\
  \tb-vc\n\
  \td-yn\n\
  \"

day23 :: AOCSolution
day23 input = [part1, part2] <*> pure (parseInput input)

parseInput :: String -> M.HashMap String [String]
parseInput = M.map sort . foldr1 (M.unionWith (<>)) . parse p
  where
    p =
      P.many1 do
        x <- P.many1 P.letter <* P.char '-'
        y <- P.many1 P.letter <* P.try P.newline
        pure $ M.fromList [(x, [y]), (y, [x])]

part1 :: M.HashMap String [String] -> String
part1 = show . countTrue (any ((== 't') . head)) . getAllCombinations
  where
    getCombinations :: M.HashMap String [String] -> String -> [[String]]
    getCombinations i v =
      filter isInterConnected $
        concat $
          init $
            zipWith
              (\x y -> map (\y' -> [v, x, y']) y)
              xs
              (drop 1 $ tails xs)
      where
        xs = i M.! v
        isInterConnected [a, b, c] = c `elem` (i M.! b) && a < b && b < c
    getAllCombinations :: M.HashMap String [String] -> [[String]]
    getAllCombinations i = concatMap (getCombinations i) $ M.keys i

part2 :: M.HashMap String [String] -> String
part2 i = intercalate "," $ sort $ splitGraph M.! k
  where
    k = maximum $ M.keys splitGraph
    splitGraph = foldr1 (M.unionWith (<>)) $ map (\x -> M.singleton (getPairs $ i M.! x) [x]) ks
    ks = M.keys i
    getPairs xs =
      countTrue id $
        concat $
          zipWith
            (\x y -> map (\y' -> elem y' $ i M.! x) y)
            xs
            (drop 1 $ tails xs)
