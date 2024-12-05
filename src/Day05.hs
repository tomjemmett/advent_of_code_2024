module Day05 (day05, day05TestInput) where

import Common
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (partition)
import Data.List.Split (splitOn)

type Rules = M.HashMap Int (S.HashSet Int)

type Update = [Int]

type Updates = [Update]

day05TestInput :: String
day05TestInput =
  "47|53\n\
  \97|13\n\
  \97|61\n\
  \97|47\n\
  \75|29\n\
  \61|13\n\
  \75|53\n\
  \29|13\n\
  \97|29\n\
  \53|29\n\
  \61|53\n\
  \97|53\n\
  \61|29\n\
  \47|13\n\
  \75|47\n\
  \97|75\n\
  \47|61\n\
  \75|61\n\
  \47|29\n\
  \75|13\n\
  \53|13\n\
  \\n\
  \75,47,61,53,29\n\
  \97,61,53,29,13\n\
  \75,29,13\n\
  \75,97,47,61,53\n\
  \61,13,29\n\
  \97,13,75,29,47\n\
  \"

day05 :: AOCSolution
day05 input = show . solve <$> [p1, map (sort rules) p2]
  where
    (rules, updates) = parseInput input
    (p1, p2) = partition (checkUpdate rules) updates
    solve = sum . map getMiddle
      where
        getMiddle xs = xs !! (length xs `div` 2)

parseInput :: String -> (Rules, Updates)
parseInput input = (createRules rules, updates)
  where
    [a, b] = lines <$> splitOn "\n\n" input
    rules = map (tuplify2 . map read . splitOn "|") a
    updates = map commaSeparatedInts b

createRules :: [(Int, Int)] -> Rules
createRules = foldr fn M.empty
  where
    fn :: (Int, Int) -> M.HashMap Int (S.HashSet Int) -> Rules
    fn (k, v) = M.insertWith mappend k (S.singleton v)

checkUpdate :: Rules -> Update -> Bool
checkUpdate rules = f
  where
    f [] = True
    f (x : xs) = all (g x) xs && f xs
    g :: Int -> Int -> Bool
    g x y = case S.member x <$> rules M.!? y of
      Just True -> False
      _ -> True

sort :: Rules -> Update -> Update
sort rules update = f update []
  where
    f [] ys = reverse ys
    f (x : xs) ys = g xs
      where
        g [] = f xs (x : ys)
        g (v : vs) = case S.member x <$> rules M.!? v of
          Just True -> f (reverse (v : ys) ++ (x : filter (/= v) xs)) []
          _ -> g vs