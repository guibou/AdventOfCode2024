module Day02 where

import Utils
import qualified Data.Text as Text

fileContent = parseContent $(getFile)

parseContent :: Text -> [[Int]]
parseContent = fmap (fmap (read . Text.unpack) . Text.words) . Text.lines

-- * Generics
isSafe l = do
  let
    sl = sort l
    deltas = zipWith subtract l (drop 1 l)
  (l == sl || l == reverse sl) && all (\x -> abs x <= 3 && abs x >= 1) deltas


-- * FIRST problem
day items = length (filter isSafe items)

isSafe' l = any isSafe (allButOne l)

-- * SECOND problem
day' items = length (filter isSafe' items)

ex = parseContent [str|\
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
|]

allButOne l = l:go l
  where
    go (x:xs) = xs: ((x:) <$> go xs)
    go [] = []

-- started at Wed Dec  4 08:23:17 AM +04 2024
-- 518 too high. First star at 8:30, just because I did not realized that >= 1
-- 8:33 second star
