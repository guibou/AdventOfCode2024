module Day11 where

import Utils

fileContent = [0 :: Int, 44, 175060, 3442, 593, 54398, 9, 8101095]

blink 0 = [1]
blink x
  | even ds = [read $ take (ds `div`2) digits, read $ drop (ds `div` 2) digits]
  | otherwise = [x * 2024]
  where
    digits = show x
    ds = length digits

blinksFix :: (Int -> Int -> Int) -> Int -> Int -> Int
blinksFix _ 0 _ = 1
blinksFix f n x = do
  sum $ do
    x' <- blink x
    pure $ f (n - 1) x'

blinks = memoFix2 blinksFix

parseContent = undefined

-- * Generics

-- * FIRST problem
day l = sum (map (blinks 25) l)

-- * SECOND problem
day' l = sum (map (blinks 75) l)

-- ex = [0, 1, 10, 99, 999] :: [Int]
ex = [125 :: Int, 17]

-- started at Thu Dec 12 05:33:52 PM +04 2024
