module Day22 (day22, day22TestInput) where

import Common
import Data.Bits (Bits (xor, (.|.), (.&.)), shiftL)
import Data.IntMap qualified as M

day22TestInput :: String
day22TestInput =
  "1\n\
  \10\n\
  \100\n\
  \2024\n\
  \"

day22 :: AOCSolution
day22 input = show <$> ([part1, part2] <*> pure evolutions)
  where
    i = parseInput input
    !evolutions = map (take 2000 . iterate evolve) i

parseInput :: String -> [Int]
parseInput = map read . lines

part1 :: [[Int]] -> Int
part1 = sum . (!! 2000)

part2 :: [[Int]] -> Int
part2 = maximum . M.elems . foldr (M.unionWith (+) . go . map (`mod` 10)) M.empty
  where
    go xs = M.fromList $ reverse $ zip ks $ drop 4 xs
      where
        ds = zipWith (-) (tail xs) xs
        ks = drop 4 $ scanl fn 0 ds
          where
            fn k x = ((shiftL k 5) .|. (x + 9)) .&. 0xFFFFF

evolve :: Int -> Int
evolve i = c
  where
    a = prune $ i `xor` (i * 64)
    b = prune $ a `xor` (a `div` 32)
    c = prune $ b `xor` (b * 2048)
    prune = (.&. 16777215)
