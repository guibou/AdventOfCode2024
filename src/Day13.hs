{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Day13 where

import Utils
import Control.Applicative (some)
import Debug.Trace (traceShow)
import Data.SBV
import Data.SBV.Internals (SMTModel (modelAssocs), CVal (..), CV (cvVal))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)

fileContent = parseContent $(getFile)

parseContent = unsafeParse $
  some parseBehavior

data Machine = Machine {
  buttonA :: V2 Int,
  buttonB :: V2 Int,
  prize :: V2 Int
  }
  deriving (Show)

parseBehavior = do
  a <- parseButton "A"
  b <- parseButton "B"
  void "Prize: X="
  x <- parseNumber
  void ", Y="
  y <- parseNumber
  void "\n"
  void "\n"
  pure $ Machine a b (V2 x y)

parseButton label = do
  void "Button "
  void label
  void ": X+"
  x <- parseNumber
  void ", Y+"
  y <- parseNumber
  void "\n"
  pure $ V2 x y

-- * Generics
solveMachine :: Machine -> [(Int, (Int, Int))]
solveMachine Machine{..} = do
  aPress <- [0..100]
  bPress <- [0..100]

  guard $ aPress *^ buttonA + bPress *^ buttonB == prize
  let cost = aPress * 3 + bPress * 1
  pure (cost, (aPress, bPress))

optimiseMachine :: Machine -> IO OptimizeResult
optimiseMachine Machine{prize = V2 prizeX prizeY,
                      buttonA = V2 buttonAX buttonAY,
                      buttonB = V2 buttonBX buttonBY} = optimize Lexicographic $ do

  aPress <- sInteger "aPress"
  bPress <- sInteger "bPress"

  constrain $ aPress * fromIntegral buttonAX + bPress * fromIntegral buttonBX .== fromIntegral prizeX
  constrain $ aPress * fromIntegral buttonAY + bPress * fromIntegral buttonBY .== fromIntegral prizeY

  minimize "tokens" $ aPress * 3 + bPress * 1
 
solveMachineSBV m = do
  res <- optimiseMachine m
  case res of
    LexicographicResult (Satisfiable _ model) -> pure $ Just $ cvIntToInteger $ (cvVal (Map.fromList (modelAssocs model) Map.! "tokens"))
    _ -> pure Nothing

pickMinimumWeight [] = 0
pickMinimumWeight [x] = x
pickMinimumWeight l = traceShow (length l) $ minimum l

-- * FIRST problem
day machines = sum $ map (pickMinimumWeight . map fst) $ map solveMachine machines

upPrice m = m { prize = m.prize + (fromIntegral @Int 10000000000000) }
upPrices = map upPrice

cvIntToInteger (CInteger i) = i
cvIntToInteger _ = error "I know that I have integer"


-- * SECOND problem
day' :: [Machine] -> IO Integer
day' machines = (sum . catMaybes) <$> mapM solveMachineSBV (upPrices machines)

ex = parseContent [str|\
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279

|]

-- started at Fri Dec 13 08:57:25 AM +04 2024
{-

 pX = a * aX + b * bX
 pY = a * aY + b * bY
 
 a et b?

 


 pX = a * aX + b * bX
 
 a = (pX - b * bX) / aX

 (on inject)
 pY = a * aY + b * bY
 pY = aY * (pX - b * bX) / aX + b * bY

 pY - aY * pX / aX = b (bY - aY * bX / aX)

 b = (pY - aY * pX) / (bY - aY * bX / aX)

-}




