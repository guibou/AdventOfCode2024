module Day04 where

import Utils
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Maybe

fileContent = parseContent $(getFile)

parseContent content = Map.fromList $ do
  (lIdx, l) <- zip [0..] (Text.lines content)
  (cIdx, c) <- zip [9..] (Text.unpack l)
  pure $ ((lIdx, cIdx), c)

-- * Generics
readWord (x, y) (dx, dy) m = sequenceA $ map (\d -> Map.lookup (x + dx * d, y + dy * d)  m) [0..3]

-- * FIRST problem
day content = sum $ do
  pos <- Map.keys content
  deltas <- [(1, 0),
             (-1, 0),
             (0, 1), (0, -1),
             (1, 1), (-1, -1), (-1, 1), (1, -1)]
  case readWord pos deltas content of
    Just "XMAS" -> pure 1
    _ -> pure 0

readWord' (x, y) m = sequenceA $ map (\d -> Map.lookup (x + d, y + d)  m) [-1..1]
readWord'' (x, y) m = sequenceA $ map (\d -> Map.lookup (x +  d, y - d)  m) [-1..1]


-- * SECOND problem
day' content = sum $ do
  pos <- Map.keys content

  let w0 = readWord' pos content
  let w1 = readWord'' pos content
  case (,) <$> w0 <*> w1 of
    Nothing -> pure 0
    (Just (a, b))
      | (a == "MAS" || a == "SAM") && (b == "MAS" || b == "SAM") -> pure 1
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