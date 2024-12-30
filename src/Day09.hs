module Day09 (day09, day09TestInput) where

import Common
import Data.Char (digitToInt)
import Data.Maybe (catMaybes)
import Debug.Trace

data Disk = Space Int | File Int Int deriving (Show)

day09TestInput :: String
day09TestInput = "2333133121414131402"

day09 :: AOCSolution
day09 input = show <$> ([part1, part2] <*> pure (takeWhile (/='\n') input))

part1 :: String -> Int
part1 i = sum $ zipWith (*) [0 ..] $ take (length r) $ fillDisk d r
  where
    r = reverse (catMaybes d)
    d = makeDisk i
    makeDisk = mkDisk 0
      where
        mkDisk _ [] = []
        mkDisk i [x] = createFile x $ Just i
        mkDisk i (x : y : xs) = file ++ space ++ mkDisk (succ i) xs
          where
            space = createFile y Nothing
            file = createFile x $ Just i
        createFile v = replicate (digitToInt v)
    fillDisk :: [Maybe Int] -> [Int] -> [Int]
    fillDisk [] _ = []
    fillDisk (d : ds) (r : rs) = case d of
      Just v -> v : fillDisk ds (r : rs)
      Nothing -> r : fillDisk ds rs

part2 :: String -> Int
part2 i = result 0 $ foldl fn d r
  where
    d = makeDisk i
    r = reverse d
    makeDisk :: String -> [Disk]
    makeDisk = mkDisk 0 . map digitToInt
      where
        mkDisk i [] = []
        mkDisk i [x] = [File i x]
        mkDisk i (x : y : xs) = File i x : Space y : mkDisk (succ i) xs
    fn :: [Disk] -> Disk -> [Disk]
    fn disk (Space _) = disk
    fn disk (File j r) = f [] disk
      where
        f sx [] = reverse sx
        f sx (Space s : xs) | r <= s = reverse (Space (s - r) : File j r : sx) ++ map (fileToSpace j) xs
        f sx xs@(File i _ : _) | i == j = reverse sx ++ xs
        f sx (x : xs) = f (x : sx) xs
        fileToSpace j (File i s) | j == i = Space s
        fileToSpace _ file = file
    result :: Int -> [Disk] -> Int
    result _ [] = 0
    result n (File i s : xs) = sum [i * v | v <- [n .. (n + s) - 1]] + result (n + s) xs
    result n (Space s : xs) = result (n + s) xs
