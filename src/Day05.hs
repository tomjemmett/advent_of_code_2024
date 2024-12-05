module Day05 (day05, day05TestInput) where

import Common
import Data.HashMap.Strict qualified as M
import Data.List (partition, sortBy)
import Data.List.Split (splitOn)

type Rules = Int -> Int -> Ordering

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
day05 input = show . sum <$> res
  where
    (rules, updates) = parseInput input
    sortedUpdates = map (sortBy rules) updates
    -- compare each update with it's sorted self
    -- if they are equal, thats the input to part 1
    -- if they aren't, thats the input to part 2
    res =
      map2 (getMiddle . snd) . untuplify2 . partition (uncurry (==)) $
        zip updates sortedUpdates
    getMiddle xs = xs !! (length xs `div` 2)

parseInput :: String -> (Rules, Updates)
parseInput input = (rules, updates)
  where
    [a, b] = lines <$> splitOn "\n\n" input
    rules = createRules $ map (tuplify2 . map read . splitOn "|") a
    updates = map commaSeparatedInts b

createRules :: [(Int, Int)] -> Rules
createRules = compareRules . foldr fn M.empty
  where
    fn (a, b) = M.insert (b, a) False . M.insert (a, b) True
    compareRules rules a b = case M.lookup (a, b) rules of
      Just True -> LT
      Just False -> GT
      Nothing -> EQ