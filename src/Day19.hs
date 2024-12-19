module Day19 where

import Utils
import qualified Data.Text as Text

fileContent = parseContent $(getFile)

parseContent t = do
  let (towels, designs) = case Text.lines t of
        (towels:_:designs) -> (towels, designs)
        _ -> error $ "File not correctly formatted"
  (map Text.unpack $ Text.splitOn ", " towels, map Text.unpack designs)

-- * Generics

-- * FIRST problem
day (towels, designs) = length $ filter (\design -> validateDesign towels design > 0) designs

validateDesign :: [String] -> String -> Int
validateDesign towels = memoFix goFix
  where
    goFix _ "" = 1
    goFix f design = sum $ do
      towel <- towels
      guard $ towel `isPrefixOf` design
      pure $ f (drop (length towel) design)

-- * SECOND problem
day' (towels, designs) = sum $ map (\design -> validateDesign towels design) designs

ex = parseContent [str|\
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
|]

-- started at Thu Dec 19 12:35:27 PM +04 2024
