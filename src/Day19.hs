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
day (towels, designs) = length $ filter count designs
  where
    count = countDesign towels

class OneSum t where
  one :: t
  reduce :: [t] -> t

instance OneSum Bool where
  one = True
  -- This shortcut, so the first valid result stops the computation
  reduce = or

instance OneSum Int where
  one = 1
  reduce = sum

countDesign :: OneSum t => [String] -> String -> t
countDesign towels = memoFix goFix
  where
    goFix _ "" = one
    goFix f design = reduce $ do
      towel <- towels
      guard $ towel `isPrefixOf` design
      pure $ f (drop (length towel) design)

-- * SECOND problem
day' (towels, designs) = sum $ map validate designs
  where
    -- Share the cache between ALL invocations
    validate = countDesign @Int towels

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
