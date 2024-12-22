module Day22 where

import Utils
import qualified Data.Text as Text
import Data.Bits
import Data.List
import qualified Data.Map as Map
import Data.Ord (comparing)

fileContent = parseContent $(getFile)

parseContent = map (read @Int . Text.unpack) . Text.lines

-- * Generics
evolve secret = do
  let step1 = prune (mix secret (secret * 64))
  let step2 = prune (mix step1 (step1 `div` 32))
  let step3 = prune (mix step2 (step2 * 2048))
  step3


-- * Operations
mix a b = xor a b
prune secret = secret `mod` 16777216

-- * FIRST problem
day numbers = sum (map (applyN 2000 evolve) numbers)

-- 2001 because iterate keeps the first item (that we want), however the rule
-- states that they generates 2000 addittionnal secret numbers.
--
-- Don't be like guibou, do not set 2000 here... ;)
secretNumbers numbers = take 2001 (iterate evolve numbers)

sellPrice numbers = map (`mod` 10) $ secretNumbers numbers

sellSequence numbers = do
  let prices = sellPrice numbers
  zip (drop 4 prices) $ (map (take 4) $ tails $ zipWith (-) (drop 1 prices) prices)

computeSequencePrices numbers = foldl' f mempty (sellSequence numbers)
  where
    f m (price, seq)
       | seq `Map.member` m = m
       | otherwise = Map.insert seq price m

-- * SECOND problem
day' numbers = do
  let theMapOfPrices = Map.unionsWith (+) $ map computeSequencePrices numbers
  snd $ maximumBy (comparing snd) (Map.toList theMapOfPrices)

ex = parseContent [str|\
1
10
100
2024
|]

ex' = parseContent [str|\
1
2
3
2024
|]

-- 2242 is too LOW

-- started at Sun Dec 22 08:13:04 PM +04 2024
