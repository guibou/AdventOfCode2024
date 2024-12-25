module Day13 where

import Utils

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
  -- pX = aPress * aX + bPress * bX (1)
  -- pY = aPress * aY + bPress * bY (2)
  --
  -- aY * (1) - aX * (2)
  --
  -- pX * aY - pY * aX = bPress * (bX * aY - bY * aX)
  --
  -- ==> bPress = (pX * aY - pY * aX) / (bX * aY - bY * aX)
  let (bPress, bMod) = (prize.x * buttonA.y - prize.y * buttonA.x) `divMod` (buttonB.x * buttonA.y - buttonB.y * buttonA.x)
  guard (bMod == 0)
  --
  -- aPress = (pX - bPress * bX) / aX
  let (aPress, aMod) = (prize.x - bPress * buttonB.x) `divMod` (buttonA.x)
  guard (aMod == 0)
  
  guard $ aPress *^ buttonA + bPress *^ buttonB == prize
  let cost = aPress * 3 + bPress * 1
  pure (cost, (aPress, bPress))

pickMinimumWeight [] = 0
pickMinimumWeight [x] = x
pickMinimumWeight l = minimum l

-- * FIRST problem
day machines = sum $ map (pickMinimumWeight . map fst) $ map solveMachine machines

upPrice m = m { prize = m.prize + (fromIntegral @Int 10000000000000) }
upPrices = map upPrice

-- * SECOND problem
day' :: [Machine] -> Int
day' machines = day (upPrices machines)

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
