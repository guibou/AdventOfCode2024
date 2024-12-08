module Day07 where

import Utils
import Control.Applicative

fileContent = parseContent $(getFile)

parseContent = unsafeParse $ do
  some $ do
    n <- parseNumber @Int
    void ": "
    res <- some (parseNumber @Int)
    void "\n"
    pure (n, res)

-- * Generics
evalExpr (resValue, numbers) = resValue `elem` possible_values
  where
    possible_values = go (reverse numbers)
    go [] = error "impossible case"
    go [x] = [x]
    go (x:xs) = do
      res' <- go xs
      [x * res', x + res']

evalExpr' (resValue, numbers) = resValue `elem` possible_values
  where
    possible_values = go (reverse numbers)
    go [] = error "impossible case"
    go [x] = [x]
    go (x:xs) = do
      res' <- go xs
      [x * res', x + res', read (show res' <> show x)]

-- * FIRST problem
day = sum @[] @Int . map fst . filter evalExpr

-- * SECOND problem
day' = sum @[] @Int . map fst . filter evalExpr'

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
