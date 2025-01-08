module Day24 (day24, day24TestInput) where

import Common
import Control.Monad.RWS
import Data.Bits (xor, (.&.), (.|.))
import Data.HashMap.Strict qualified as M
import Data.List (intercalate, sort)
import Data.List.Split (splitOn)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

type Wires = M.HashMap String Int

type Gates = M.HashMap String (String, String, String)

day24TestInput :: String
day24TestInput =
  "x00: 1\n\
  \x01: 0\n\
  \x02: 1\n\
  \x03: 1\n\
  \x04: 0\n\
  \y00: 1\n\
  \y01: 1\n\
  \y02: 1\n\
  \y03: 1\n\
  \y04: 1\n\
  \\n\
  \ntg XOR fgs -> mjb\n\
  \y02 OR x01 -> tnw\n\
  \kwq OR kpj -> z05\n\
  \x00 OR x03 -> fst\n\
  \tgd XOR rvg -> z01\n\
  \vdt OR tnw -> bfw\n\
  \bfw AND frj -> z10\n\
  \ffh OR nrd -> bqk\n\
  \y00 AND y03 -> djm\n\
  \y03 OR y00 -> psh\n\
  \bqk OR frj -> z08\n\
  \tnw OR fst -> frj\n\
  \gnj AND tgd -> z11\n\
  \bfw XOR mjb -> z00\n\
  \x03 OR x00 -> vdt\n\
  \gnj AND wpb -> z02\n\
  \x04 AND y00 -> kjc\n\
  \djm OR pbm -> qhw\n\
  \nrd AND vdt -> hwm\n\
  \kjc AND fst -> rvg\n\
  \y04 OR y02 -> fgs\n\
  \y01 AND x02 -> pbm\n\
  \ntg OR kjc -> kwq\n\
  \psh XOR fgs -> tgd\n\
  \qhw XOR tgd -> z09\n\
  \pbm OR djm -> kpj\n\
  \x03 XOR y03 -> ffh\n\
  \x00 XOR y04 -> ntg\n\
  \bfw OR bqk -> z06\n\
  \nrd XOR fgs -> wpb\n\
  \frj XOR qhw -> z04\n\
  \bqk OR frj -> z07\n\
  \y03 OR x01 -> nrd\n\
  \hwm AND bqk -> z03\n\
  \tgd XOR rvg -> z12\n\
  \tnw OR pbm -> gnj\
  \"

day24 :: AOCSolution
day24 input = [show p1, p2]
  where
    (w, g) = parseInput input
    zs = sort $ filter ((== 'z') . head) $ M.keys g
    p1 = run w g zs
    lastZ = maximum zs
    p2 = intercalate "," $ sort $ findIncorrect lastZ (M.elems g) (M.toList g)

parseInput :: String -> (Wires, Gates)
parseInput = parse do
  a <- pWire `P.sepEndBy` P.newline
  P.newline
  b <- pGate `P.sepEndBy` P.newline
  pure (M.fromList a, M.fromList b)

pWire :: Parser (String, Int)
pWire = do
  n <- pName <* P.string ": "
  v <- read <$> P.many1 P.digit
  pure (n, v)

pGate :: Parser (String, (String, String, String))
pGate = do
  a <- pName <* P.space
  o <- P.choice $ P.try . P.string <$> ["AND", "XOR", "OR"]
  P.space
  b <- pName <* P.string " -> "
  c <- pName
  pure (c, (o, a, b))

pName :: Parser String
pName = P.many1 P.alphaNum

evalInstruction :: String -> RWS Gates [Int] Wires Int
evalInstruction x = do
  m <- get
  case m M.!? x of
    Just v -> pure v
    Nothing -> do
      ops <- ask
      let (o, a, b) = ops M.! x
      a' <- evalInstruction a
      b' <- evalInstruction b
      let r = getOp o a' b'
      modify $ M.insert x r
      pure r
  where
    getOp :: String -> (Int -> Int -> Int)
    getOp = \case
      "AND" -> (.&.)
      "OR" -> (.|.)
      "XOR" -> xor

run :: Wires -> Gates -> [String] -> Int
run w g xs = binaryToDecimal $ reverse $ fst $ evalRWS (traverse evalInstruction xs) g w

binaryToDecimal :: [Int] -> Int
binaryToDecimal = foldl (\acc bit -> acc * 2 + bit) 0

findIncorrect :: String -> [(String, String, String)] -> [(String, (String, String, String))] -> [String]
findIncorrect _ _ [] = []
findIncorrect lastZ g ((k@(k' : _), (o, a@(a' : _), b@(b' : _))) : gs)
  | k' == 'z' && o /= "XOR" && k /= lastZ = k : findIncorrect lastZ g gs
  | o == "XOR" && all (`notElem` "xyz") [k', a', b'] = k : findIncorrect lastZ g gs
  | o == "XOR" && any (f1 k) g = k : findIncorrect lastZ g gs
  | o == "AND" && "x00" `notElem` [a, b] && any (f2 k) g = k : findIncorrect lastZ g gs
  | otherwise = findIncorrect lastZ g gs
  where
    f1 k (o', a', b') = o' == "OR" && k `elem` [a', b']
    f2 k (o', a', b') = o' /= "OR" && k `elem` [a', b']