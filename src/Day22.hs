module Day22 (day22, day22TestInput) where

import Common
import Control.Monad (unless)
import Data.Bits (Bits (xor, (.&.), (.|.)), shift)
import Data.Foldable (for_)
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as MV

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
    i = map read $ lines input
    evolutions = map (take 2000 . iterate evolve) i

part1 :: [[Int]] -> Int
part1 = sum . (!! 2000)

part2 :: [[Int]] -> Int
part2 input = V.maximum $ V.create do
  v <- MV.replicate 0x100000 0
  for_ (map2 (`mod` 10) input) \xs -> do
    let ds = zipWith (-) (tail xs) xs
        ks = scanl (\k x -> (shift k 5 .|. (x + 9)) .&. 0xFFFFF) 0 ds
    s <- MV.replicate 0x100000 False
    for_ (drop 4 $ zip ks xs) \(k, x) ->
      MV.exchange s k True >>= flip unless (MV.modify v (+ x) k)
  pure v

evolve :: Int -> Int
evolve i = foldr f i [11, -5, 6]
  where
    f s x = x `xor` (x `shift` s) .&. 16777215
