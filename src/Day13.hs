{-# LANGUAGE RecordWildCards #-}
module Day13 where

import Utils
import Control.Applicative (some)

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
  "Prize: X="
  x <- parseNumber
  ", Y="
  y <- parseNumber
  "\n"
  "\n"
  pure $ Machine a b (V2 x y)

parseButton label = do
  "Button "
  label
  ": X+"
  x <- parseNumber
  ", Y+"
  y <- parseNumber
  "\n"
  pure $ V2 x y

-- * Generics
solveMachine :: Machine -> _
solveMachine Machine{..} = do
  aPress <- [0..100]
  bPress <- [0..100]

  guard $ aPress *^ buttonA + bPress *^ buttonB == prize
  let cost = aPress * 3 + bPress * 1
  pure (cost, (aPress, bPress))

foo [] = 0
foo l = minimum l

-- * FIRST problem
day machines = sum $ map (foo . map fst) $ map solveMachine machines

-- * SECOND problem
day' = undefined

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
