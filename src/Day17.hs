{-# LANGUAGE TemplateHaskell #-}

-- module Day17 (day17, day17TestInput) where
module Day17 where

import Common
import Control.Lens
import Data.Bits
import Data.List (intercalate)
import Data.Vector qualified as V
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

type Instructions = V.Vector Int

type MachineState = (Registers, Int, [Int])

data Registers = Registers
  { _regA :: Int,
    _regB :: Int,
    _regC :: Int
  }
  deriving (Show)

makeRegisters :: Int -> Int -> Int -> Registers
makeRegisters a b c =
  Registers
    { _regA = a,
      _regB = b,
      _regC = c
    }

makeLenses ''Registers

day17TestInput :: String
day17TestInput =
  "Register A: 729\n\
  \Register B: 0\n\
  \Register C: 0\n\
  \\n\
  \Program: 0,1,5,4,3,0\n\
  \"

day17 :: AOCSolution
day17 input = [p1]
  where
    i = parseInput input
    p1 = getOutput $ runInstructions i

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
  0 -> 0
  1 -> 1
  2 -> 2
  3 -> 3
  4 -> r ^. regA
  5 -> r ^. regB
  6 -> r ^. regC
  7 -> undefined

getOutput :: MachineState -> String
getOutput (_, _, output) = intercalate "," $ map show $ reverse output

runInstructions :: (Registers, V.Vector Int) -> MachineState
runInstructions (regs, instrs) = until halt (runInstruction instrs) (regs, 0, [])
  where
    halt (_, p, _) = p >= V.length instrs

runInstruction :: V.Vector Int -> MachineState -> MachineState
runInstruction instrs (regs, ptr, output) = case i of
  0 -> (adv regs o, ptr', output)
  1 -> (bxl regs o, ptr', output)
  2 -> (bst regs o, ptr', output)
  3 -> (regs, if regs ^. regA == 0 then ptr' else o, output)
  4 -> (bxc regs o, ptr', output)
  5 -> (regs, ptr', out regs o : output)
  6 -> (bdv regs o, ptr', output)
  7 -> (cdv regs o, ptr', output)
  where
    i = instrs V.! ptr
    o = instrs V.! succ ptr
    ptr' = ptr + 2
    adv :: Registers -> Int -> Registers
    adv r o = set regA (r ^. regA `div` 2 ^ combo r o) r
    bxl :: Registers -> Int -> Registers
    bxl r o = set regB (r ^. regB `xor` o) r
    bst :: Registers -> Int -> Registers
    bst r o = set regB (combo r o `mod` 8) r
    bxc :: Registers -> p -> Registers
    bxc r _ = set regB (r ^. regB `xor` r ^. regC) r
    out :: Registers -> Int -> Int
    out r o = combo r o `mod` 8
    bdv :: Registers -> Int -> Registers
    bdv r o = set regB (r ^. regA `div` 2 ^ combo r o) r
    cdv :: Registers -> Int -> Registers
    cdv r o = set regC (r ^. regA `div` 2 ^ combo r o) r

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