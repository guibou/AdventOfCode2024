module Day03 where

import Data.Array
import Text.Regex.TDFA
import Utils
import qualified Data.Text as Text

fileContent = $(getFile)

-- * Generics

-- * FIRST problem
toMul x = 
        let (a, b) = ( read $
                    Text.unpack $
                      fst $
                        x ! (1 :: Int),
                  read $ Text.unpack $ fst $ x ! 2
                )
        in a * b


day :: Text -> Int
day content = sum $ map (\(a, b) -> a * b) $ 
  map
    ( \x ->
        ( read $
            Text.unpack $
              fst $
                x ! (1 :: Int),
          read $ Text.unpack $ fst $ x ! 2
        )
    )
    ((content =~ reg) :: [MatchText Text])
  where
    reg = "mul\\(([0-9]+),([0-9]+)\\)" :: String

-- * SECOND problem

day' :: Text -> Int
day' content = 
    go 0 True (((content =~ reg) :: [MatchText Text]))
  where
    reg = "do\\(\\)|don't\\(\\)|mul\\(([0-9]+),([0-9]+)\\)" :: String

    go !acc _enable [] = acc
    go !acc enable (x:xs) = case fst (x ! 0) of
       "do()" -> go acc True xs
       "don't()" -> go acc False xs
       _
         | not enable -> go acc enable xs
         | otherwise -> go (acc + toMul x) enable xs


ex = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" :: Text

-- started at Wed Dec  4 08:34:20 AM +04 2024
-- First star 8:50. Seriously, I should learn regex api in haskell
-- Secnod star 8:57
