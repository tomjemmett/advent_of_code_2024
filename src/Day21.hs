module Day21 (day21, day21TestInput) where

import Common
import Data.HashMap.Strict qualified as M

day21TestInput :: String
day21TestInput =
  "029A\n\
  \980A\n\
  \179A\n\
  \456A\n\
  \379A\n\
  \"

day21 :: AOCSolution
day21 input = show . p <$> [2, 25]
  where
    p n = sum $ map (solve n) $ lines input

solve :: Int -> String -> Int
solve n s = sum (M.elems $ go (succ n) k) * read (init s)
  where
    k = keypad s
    go :: Int -> M.HashMap (Point2d, Bool) Int -> M.HashMap (Point2d, Bool) Int
    go 0 r = r
    go n r = go (pred n) $ foldr1 (M.unionWith (+)) rs
      where
        xs = map getStr $ M.keys r
        is = M.elems r
        rs = zipWith dirpad is xs

getStr :: (Point2d, Bool) -> String
getStr ((x, y), f) = (if f then reverse else id) (ls ++ ds ++ us ++ rs) ++ "A"
  where
    ls = replicate (-x) '<'
    rs = replicate x '>'
    us = replicate (-y) '^'
    ds = replicate y 'v'

keypad :: String -> M.HashMap (Point2d, Bool) Int
keypad = run d 1 M.empty (d M.! 'A')
  where
    d = M.fromList [(c, (i `mod` 3, i `div` 3)) | (i, c) <- zip [0 ..] "789456123 0A"]

dirpad :: Int -> String -> M.HashMap (Point2d, Bool) Int
dirpad i = run d i M.empty (d M.! 'A')
  where
    d = M.fromList [(c, (i `mod` 3, i `div` 3)) | (i, c) <- zip [0 ..] " ^A<v>"]

run ::
  M.HashMap Char Point2d ->
  Int ->
  M.HashMap (Point2d, Bool) Int ->
  Point2d ->
  String ->
  M.HashMap (Point2d, Bool) Int
run _ _ ctr _ [] = ctr
run d i ctr (px, py) (s : ss) = run d i ctr' (nx, ny) ss
  where
    (ex, ey) = d M.! ' '
    (nx, ny) = d M.! s
    (dx, dy) = (nx - px, ny - py)
    f = (nx == ex && py == ey) || (ny == ey && px == ex)
    ctr' = M.insertWith (+) ((dx, dy), f) i ctr