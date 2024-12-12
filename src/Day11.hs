module Day11 where

import Utils

fileContent = [0 :: Int, 44, 175060, 3442, 593, 54398, 9, 8101095]

blinksFix :: (Int -> Int -> Int) -> Int -> Int -> Int
blinksFix _ 0 _ = 1
blinksFix f n 0 = f (n - 1) 1
blinksFix f n x
  | even ds = f (n - 1) a + f (n - 1) b
  | otherwise = f (n - 1) (x * 2024)
  where
    ds = nbDigits x
    (a, b) = x `divMod` (10 ^ (ds `div` 2))

nbDigits :: Int -> Int
nbDigits x = go x 0
  where
    go 0 !n = n
    go v n = go (v `div` 10) (n + 1)

blinks = memoFix2 blinksFix

-- * Generics

-- * FIRST problem
day l = sum (map (blinks 25) l)

-- * SECOND problem
day' l = sum (map (blinks 75) l)

-- ex = [0, 1, 10, 99, 999] :: [Int]
ex = [125 :: Int, 17]

-- started at Thu Dec 12 05:33:52 PM +04 2024
