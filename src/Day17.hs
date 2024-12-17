{-# LANGUAGE TemplateHaskell #-}

module Day17 (day17, day17TestInput) where

import Common
import Control.Lens (makeLenses, set, (^.))
import Data.Bits (Bits (xor))
import Data.List (intercalate)
import Data.Maybe (maybe)
import Data.Vector qualified as V
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

type Instructions = V.Vector Int

data Registers = Registers
  { _regA :: Int,
    _regB :: Int,
    _regC :: Int
  }
  deriving (Show)

data Computer = Computer
  { _registers :: Registers,
    _pointer :: Int,
    _output :: [Int]
  }
  deriving (Show)

makeRegisters :: Int -> Int -> Int -> Registers
makeRegisters a b c =
  Registers
    { _regA = a,
      _regB = b,
      _regC = c
    }

makeComputer :: Registers -> Computer
makeComputer r = Computer {_registers = r, _pointer = 0, _output = []}

makeLenses ''Registers
makeLenses ''Computer

day17TestInput :: [String]
day17TestInput = [day17TestInput1, day17TestInput2]

day17TestInput1, day17TestInput2 :: String
day17TestInput1 =
  "Register A: 729\n\
  \Register B: 0\n\
  \Register C: 0\n\
  \\n\
  \Program: 0,1,5,4,3,0\n\
  \"
day17TestInput2 =
  "Register A: 2024\n\
  \Register B: 0\n\
  \Register C: 0\n\
  \\n\
  \Program: 0,3,5,4,3,0\n\
  \"

day17 :: AOCSolution
day17 input = [part1, part2] <*> pure (parseInput input)

part1 :: (Registers, Instructions) -> String
part1 = getOutput . runInstructions
  where
    getOutput :: Computer -> String
    getOutput comp = intercalate "," $ map show $ reverse $ comp ^. output

part2 :: (Registers, Instructions) -> String
part2 (_, instructions) = maybe "-" show $ go 0 (pred sz) 0
  where
    irl = reverse $ V.toList instructions
    sz = V.length instructions
    go :: Int -> Int -> Int -> Maybe Int
    go b (-1) _ = Just b
    go b i 8 = Nothing
    go b i n
      | take (sz - i) irl == take (sz - i) r = case go a (pred i) 0 of
          Just v -> Just v
          Nothing -> go b i $ succ n
      | otherwise = go b i $ succ n
      where
        a = b + n * 8 ^ i
        r = runInstructions (makeRegisters a 0 0, instructions) ^. output

parseInput :: String -> (Registers, Instructions)
parseInput = parse do
  a <- pReg
  b <- pReg
  c <- pReg
  p <- P.string "Program: " *> (number `P.sepBy` P.char ',')
  pure (makeRegisters a b c, V.fromList p)
  where
    pReg = P.string "Register " >> P.anyChar >> P.string ": " >> number

combo :: Registers -> Int -> Int
combo r = \case
  4 -> r ^. regA
  5 -> r ^. regB
  6 -> r ^. regC
  x -> x

runInstructions :: (Registers, V.Vector Int) -> Computer
runInstructions (regs, instrs) = until halt (runInstruction instrs) init
  where
    halt computer = computer ^. pointer >= V.length instrs
    init = makeComputer regs

runInstruction :: V.Vector Int -> Computer -> Computer
runInstruction instrs comp = case i of
  0 -> rn (\r o -> set regA (r ^. regA `div` 2 ^ combo r o) r)
  1 -> rn (\r o -> set regB (r ^. regB `xor` o) r)
  2 -> rn (\r o -> set regB (combo r o `mod` 8) r)
  3 -> set pointer (if comp ^. registers . regA == 0 then ptr' else o) comp
  4 -> rn (\r _ -> set regB (r ^. regB `xor` r ^. regC) r)
  5 -> set pointer ptr' $ set output (combo (comp ^. registers) o `mod` 8 : comp ^. output) comp
  6 -> rn (\r o -> set regB (r ^. regA `div` 2 ^ combo r o) r)
  7 -> rn (\r o -> set regC (r ^. regA `div` 2 ^ combo r o) r)
  where
    ptr = comp ^. pointer
    i = instrs V.! ptr
    o = instrs V.! succ ptr
    ptr' = ptr + 2
    rn :: (Registers -> Int -> Registers) -> Computer
    rn f = set pointer ptr' $ set registers (f (comp ^. registers) o) comp

{-
Notes: what does our program do?

run a simple program
getOutput $ flip (curry runInstructions) i $  makeRegisters 0 0 0 -- => 6
getOutput $ flip (curry runInstructions) i $  makeRegisters 7 0 0 -- => 1
getOutput $ flip (curry runInstructions) i $  makeRegisters 8 0 0 -- => 7,7
getOutput $ flip (curry runInstructions) i $  makeRegisters 63 0 0 -- => 2,1
getOutput $ flip (curry runInstructions) i $  makeRegisters 64 0 0 -- => 6,7,7

so, looks like every power of 8 we add an extra digit
running with a set from 8-15 gives us values ending in ,7
then 16 movees to 4,5

so, seems like we can fix a digit by some power of 8
-}