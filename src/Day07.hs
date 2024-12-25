module Day07 where

import Utils

fileContent = parseContent $(getFile)

parseContent = unsafeParse $ do
  some $ do
    n <- parseNumber @Int
    void ": "
    res <- some (parseNumber @Int)
    void "\n"
    pure (n, res)

-- * Generics
evalExpr :: (Int -> Int -> [Int]) -> (Int, [Int]) -> Bool
evalExpr _ (_, []) = False
evalExpr makeExpr (resValue, x:numbers) = go x numbers
  where
    go :: Int -> [Int] -> Bool
    go acc [] = acc == resValue
    go acc (x:xs)
      | acc > resValue = False
      | otherwise = or $ do
          newAcc <- makeExpr acc x
          pure $ go newAcc xs

pad x 0 = x
pad x y = pad (x * 10) (y `div` 10)

-- * FIRST problem
exprDay1 x y = [x * y, x + y]

day = sum @[] @Int . map fst . filter (evalExpr $ \x y -> [x * y, x + y])

-- * SECOND problem
exprDay2 x y = pad x y + y : exprDay1 x y

-- Stupid optimisation: the lines which are valid with day1 computation do not need to be checked with day2
day' = sum @[] @Int . map fst . filter (\x -> evalExpr exprDay2 x)

ex = parseContent [str|\
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
|]

-- started at Sun Dec  8 14:57:32 PM +04 2024
