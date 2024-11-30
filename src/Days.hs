module Days
  ( day01,
    day02,
    day03,
    day04,
    day05,
    day06,
    day07,
    day08,
    day09,
    day10,
    day11,
    day12,
    day13,
    day14,
    day15,
    day16,
    day17,
    day18,
    day19,
    day20,
    day21,
    day22,
    day23,
    day24,
    day25,
    day01TestInput,
    day02TestInput,
    day03TestInput,
    day04TestInput,
    day05TestInput,
    day06TestInput,
    day07TestInput,
    day08TestInput,
    day09TestInput,
    day10TestInput,
    day11TestInput,
    day12TestInput,
    day13TestInput,
    day14TestInput,
    day15TestInput,
    day16TestInput,
    day17TestInput,
    day18TestInput,
    day19TestInput,
    day20TestInput,
    day21TestInput,
    day22TestInput,
    day23TestInput,
    day24TestInput,
    day25TestInput,
    runDay,
  )
where

import Control.Monad (when)
import Day01 (day01, day01TestInput)
import Day02 (day02, day02TestInput)
import Day03 (day03, day03TestInput)
import Day04 (day04, day04TestInput)
import Day05 (day05, day05TestInput)
import Day06 (day06, day06TestInput)
import Day07 (day07, day07TestInput)
import Day08 (day08, day08TestInput)
import Day09 (day09, day09TestInput)
import Day10 (day10, day10TestInput)
import Day11 (day11, day11TestInput)
import Day12 (day12, day12TestInput)
import Day13 (day13, day13TestInput)
import Day14 (day14, day14TestInput)
import Day15 (day15, day15TestInput)
import Day16 (day16, day16TestInput)
import Day17 (day17, day17TestInput)
import Day18 (day18, day18TestInput)
import Day19 (day19, day19TestInput)
import Day20 (day20, day20TestInput)
import Day21 (day21, day21TestInput)
import Day22 (day22, day22TestInput)
import Day23 (day23, day23TestInput)
import Day24 (day24, day24TestInput)
import Day25 (day25, day25TestInput)
import System.Directory (doesFileExist)
import System.TimeIt (timeIt)

days =
  [ (day01, "inputs/day01.txt"),
    (day02, "inputs/day02.txt"),
    (day03, "inputs/day03.txt"),
    (day04, "inputs/day04.txt"),
    (day05, "inputs/day05.txt"),
    (day06, "inputs/day06.txt"),
    (day07, "inputs/day07.txt"),
    (day08, "inputs/day08.txt"),
    (day09, "inputs/day09.txt"),
    (day10, "inputs/day10.txt"),
    (day11, "inputs/day11.txt"),
    (day12, "inputs/day12.txt"),
    (day13, "inputs/day13.txt"),
    (day14, "inputs/day14.txt"),
    (day15, "inputs/day15.txt"),
    (day16, "inputs/day16.txt"),
    (day17, "inputs/day17.txt"),
    (day18, "inputs/day18.txt"),
    (day19, "inputs/day19.txt"),
    (day20, "inputs/day20.txt"),
    (day21, "inputs/day21.txt"),
    (day22, "inputs/day22.txt"),
    (day23, "inputs/day23.txt"),
    (day24, "inputs/day24.txt"),
    (day25, "inputs/day25.txt")
  ]

runDay :: Int -> IO ()
runDay day = do
  let (fn, file) = days !! pred day
  fileExists <- doesFileExist file
  when fileExists do
    input <- readFile file

    when (input /= "") $ timeIt do
      putStrLn $ replicate 80 '-'
      putStr $ "Day: " ++ show day
      putStrLn ""
      putStr $ unlines $ fn input