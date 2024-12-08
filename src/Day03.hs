module Day03 where

import Utils
import qualified Data.Text as Text
import Control.Lens.Regex.Text
import Control.Lens ((^..))

fileContent = $(getFile)

-- * Generics

-- * FIRST problem
toMul [x, y] = 
        let (a, b) = ( read $ Text.unpack x, read $ Text.unpack y)
        in a * b
toMul s = error $ show s


day :: Text -> Int
day content = sum $ 
  map toMul ((content ^.. reg . groups) :: [[Text]])
  where
    reg = [regex|mul\(([0-9]+),([0-9]+)\)|]

-- * SECOND problem

day' :: Text -> Int
day' content = 
    go 0 True (((content ^.. reg . groups) :: [[Text]]))
  where
    reg = [regex|(do|don't)\(\)|mul\(([0-9]+),([0-9]+)\)|]

    go !acc _enable [] = acc
    go !acc _enable ([op]:xs) = case Text.unpack op of
       "do" -> go acc True xs
       "don't" -> go acc False xs
       s -> error $ show s
    go !acc enable (["", x, y]:xs)
         | not enable = go acc enable xs
         | otherwise = go (acc + toMul [x, y]) enable xs
    go !_acc _enable x = error (show x)


ex = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" :: Text

-- started at Wed Dec  4 08:34:20 AM +04 2024
-- First star 8:50. Seriously, I should learn regex api in haskell
-- Secnod star 8:57
