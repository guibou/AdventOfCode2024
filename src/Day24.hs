module Day24 where

import Utils
import Text.Megaparsec.Char
import Control.Applicative ((<|>))
import qualified Data.Map as Map
import Data.Bits (xor, Bits (..))
import Data.List (intercalate)

fileContent = parseContent $(getFile)

parseContent = unsafeParse $ do
  wires <- some parseWire
  void "\n"
  gates <- some parseGates
  pure (wires, gates)

parseWire = do
  ns <- parseWireName
  void ": "
  value <- ("1" $> True) <|> ("0" $> False)
  void "\n"
  pure (ns, value)

parseWireName = Wire <$> (some alphaNumChar)

newtype Wire = Wire String
  deriving (Show, Ord, Eq)

data Op = And | Or | Xor
  deriving (Show)

parseGates = do
  wireA <- parseWireName
  void " "
  op <- ("AND" $> And) <|> ("XOR" $> Xor) <|> ("OR" $> Or)
  void " "
  wireB <- parseWireName
  void " -> "
  wireOut <- parseWireName
  void "\n"
  pure (wireOut, (wireA, op, wireB))


-- * Generics

-- * FIRST problem
solve :: ([(Wire, Bool)], [(Wire, (Wire, Op, Wire))]) -> Map Wire Bool
solve (initialValues, circuits) = do
   let
      finalValues = Map.fromList initialValues <> (Map.fromList $ do
         (outputWire, equation) <- circuits
         pure (outputWire, solveEquation finalValues equation))
   finalValues

solveEquation :: Map Wire Bool -> (Wire, Op, Wire) -> Bool
solveEquation cache (a, op, b) = do
  let valA = cache Map.! a
  let valB = cache Map.! b
  case op of
    And -> valA .&. valB
    Or -> valA .|. valB
    Xor -> xor valA valB
--
--
day problem = do
  let res = solve problem
  wireToInt 'z' res

wireToInt :: Char -> Map Wire Bool -> Int
wireToInt c (Map.toList -> wires) = foldl' f 0 (zip [0..] bits)
  where
    bits = map snd $ sort $ filter (\(Wire name, _) -> head name == c) $ wires
    f accum (idx, True) = setBit accum idx
    f accum (_, False) = accum

intToWire :: Int -> Char -> Int -> [(Wire, Bool)]
intToWire bitWidth wireName value = do
  bitNumber <- [0..(bitWidth - 1)]
  pure (Wire $ wireName:[fmt|{bitNumber:02d}|], testBit value bitNumber)

inversions = [("z10", "vcf"),
                 ("z17", "fhg"),
                 ("dvb", "fsq"),
                 ("tnc", "z39")]

day' _ = intercalate "," $ sort $ concat $ map (\(x, y) -> [x, y]) inversions

-- * SECOND problem
-- This returns the list of where the problem may be located, by testing the
-- different bit for correct behavior
--
-- It allows an inversion map which is refined after visually looking at the problem in the dot file
testProblem inversions (wires, equations') = do
  let equationMap = Map.fromList equations'

  let invert a b m = Map.insert (Wire a) (equationMap Map.! (Wire b)) $ Map.insert (Wire b) (equationMap Map.! (Wire a)) $ m

  let equationMap' = foldl' (\m (a, b) -> invert a b m) equationMap inversions
  let equations = Map.toList equationMap'
  let nbInputBits = length wires `div` 2

  currentCheckedBit <- [0..nbInputBits - 1]

  let 
       setABit _bitPosition False = 0
       setABit bitPosition True = bit bitPosition
       oneProblem (bitX, bitY, bitZ, retZ) = do
        let wireX = intToWire nbInputBits 'x' (setABit currentCheckedBit bitX)
        let wireY = intToWire nbInputBits 'y' (setABit currentCheckedBit bitY)
        -- let resZ = wireToInt 'z' ((setABit currentCheckedBit bitZ + setABit (currentCheckedBit + 1) retZ))
        let solveRes = wireToInt 'z' (solve (wireX <> wireY, equations))
        let resBit = testBit solveRes currentCheckedBit
        let retBit = testBit solveRes (currentCheckedBit + 1)

        let resError
              | resBit /= bitZ = [("res", currentCheckedBit, (bitX, bitY, resBit))]
              | otherwise = []
        let retError
              | retBit /= retZ = [("ret", currentCheckedBit, (bitX, bitY, retBit))]
              | otherwise = []
        resError <> retError
  -- we have 4 checks

  -- 0, 0 -> (0, 0)
  -- 1, 0 -> (1, 0)
  -- 0, 1 -> (1, 0)
  -- 1, 1 -> (0, 1)
  concatMap oneProblem [
              (False, False, False, False),
              (True, False, True, False),
              (False, True, True, False),
              (True, True, False, True)]



equationToNode (Wire outputWire, (Wire a, op, Wire b)) = do
  [
    [fmt|{outputWire} [label="{outputWire} {op:s}"]|],
    [fmt|{a} -> {outputWire}|],
    [fmt|{b} -> {outputWire}|]
   ]

toNode (_wires, equations) = unlines $ 
  ["digraph {"] <>
  concatMap equationToNode equations <>
  ["}"]

ex = parseContent [str|\
x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02
|]

ex' = parseContent [str|\
x00: 0
x01: 1
x02: 0
x03: 1
x04: 0
x05: 1
y00: 0
y01: 0
y02: 1
y03: 1
y04: 0
y05: 1

x00 AND y00 -> z05
x01 AND y01 -> z02
x02 AND y02 -> z01
x03 AND y03 -> z03
x04 AND y04 -> z04
x05 AND y05 -> z00
|]

-- started at Tue Dec 24 08:59:59 AM +04 2024
