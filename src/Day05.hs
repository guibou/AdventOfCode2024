module Day05 where

import Utils
import Control.Applicative
import Text.Megaparsec (sepBy)
import qualified Data.Set as Set

fileContent :: ([(Int, Int)], [[Int]])
fileContent = parseContent $(getFile)

parseContent = unsafeParse $ do
  ordering <- some pageOrdering
  void "\n"
  updates <- some pageUpdates
  pure (ordering, updates)

pageOrdering = do
  a <- parseNumber
  void "|"
  b <- parseNumber
  void "\n"
  pure (a, b)

pageUpdates = do
  ns <- parseNumber `sepBy` ","
  void "\n"
  pure ns

-- * Slow version, I have a loop in the initial one?!

buildPairs :: [Int] -> [(Int, Int)]
buildPairs [] = []
buildPairs (x:xs) = do
  (fmap (\y -> (x, y)) xs) ++ buildPairs xs

day (deps, rules) = do
  let depsSet = Set.fromList deps
  let correctRules = filter (check depsSet) rules
  sum $ map weightRule correctRules
  
check :: Set (Int, Int) -> [Int] -> Bool
check deps l = all (\(x, y) -> not $ (y, x) `Set.member` deps) (buildPairs l)

-- * Generics

-- * FIRST problem

weightRule rule = rule !! (length rule `div` 2) 

-- * SECOND problem
day' (deps, rules) = do
  let depsSet = Set.fromList deps
  let inCorrectRules = filter (not . check depsSet) rules
  let reordered = map (reorder depsSet) inCorrectRules
  sum $ map weightRule reordered

reorder :: Set (Int, Int) -> [Int] -> [Int]
reorder depSet x = go [] x
  where
    go res [] = res
    go res (x:xs) = go (reinsert depSet x res) xs

reinsert :: (Set (Int, Int)) -> Int -> [Int] -> [Int]
reinsert _ x [] = [x]
reinsert depSet x xs@(y:ys)
  | check depSet (x:xs) = (x:xs)
  | otherwise = y:reinsert depSet x ys

ex = parseContent [str|\
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
|]

-- started at Sun Dec  8 12:09:13 PM +04 2024
