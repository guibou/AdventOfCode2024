module Day24 where

import Utils
import Text.Megaparsec.Char
import Control.Applicative ((<|>))
import qualified Data.Map as Map
import Data.Bits (xor, Bits (..))

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
  let zWires = sort $ filter (\(Wire name, _) -> head name == 'z') $ Map.toList res
  toInt (map snd zWires)

toInt :: [Bool] -> Int
toInt bits = foldl' f 0 (zip [0..] bits)
  where
    f accum (idx, True) = setBit accum idx
    f accum (_, False) = accum

-- * SECOND problem
day' = undefined

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
