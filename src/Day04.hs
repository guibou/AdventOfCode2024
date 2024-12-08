module Day04 where

import Utils
import qualified Data.Map.Strict as Map
import Control.Monad (guard)

fileContent = parseContent $(getFile)

parseContent = parse2DGrid @Char id

-- * Generics
readWord pos delta m = sequenceA $ map (\d -> Map.lookup (pos + delta ^* d)  m) [1..3]

-- * FIRST problem
day content = sum $ do
  (pos, c) <- Map.toList content
  guard $ c == 'X'
  deltas <- drop 1 connect8
  case readWord pos deltas content of
    Just "MAS" -> pure 1
    _ -> pure 0

readWord' pos m = sequenceA $ map (\d -> Map.lookup (pos + V2 d d)  m) [-1, 1]
readWord'' pos m = sequenceA $ map (\d -> Map.lookup (pos + V2 d (-d)) m) [-1, 1]


-- * SECOND problem
day' content = sum $ do
  (pos, c) <- Map.toList content
  guard $ c == 'A'

  let w0 = readWord' pos content
  let w1 = readWord'' pos content
  case (,) <$> w0 <*> w1 of
    Nothing -> pure 0
    (Just (a, b))
      | (a == "MS" || a == "SM") && (b == "MS" || b == "SM") -> pure 1
      | otherwise -> pure 0

ex = parseContent [str|\
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
|]

-- started at Wed Dec  4 08:58:05 AM +04 2024
