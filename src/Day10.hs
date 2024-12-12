module Day10 where

import Utils
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

fileContent = parseContent $(getFile)

parseContent = parse2DGrid (\x -> if x == '.' then -1 else read @Int [x])

-- * Generics

-- * FIRST problem
day heights = sum $ map (length . Set.fromList . map last) $ findAllTrails heights

findTrails :: Map (V2 Int) Int -> V2 Int -> [[V2 Int]]
findTrails heights trailHead = go trailHead 0
  where
    go currentPos 9 = pure [currentPos]
    go currentPos currentHeight = fmap (currentPos:) $ do
      deltaPos <- drop 1 connect4
      let nextPos = currentPos + deltaPos
      case Map.lookup nextPos heights of
        Nothing -> []
        Just height'
          | height' == currentHeight + 1 -> go nextPos height'
          | otherwise -> []
   
findAllTrails heights = do
  let trailHeads = map fst $ (filter (\(_pos, height) -> height == 0)) $ Map.toList heights
  map (findTrails heights) trailHeads
  

-- * SECOND problem
day' heights = sum $ map length $ findAllTrails heights

ex = parseContent [str|\
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
|]

ex' = parseContent [str|\
0123
1234
8765
9876
|]

ex'' = parseContent [str|\
...0...
...1...
...2...
6543456
7.....7
8.....8
9.....9
|]

ex''' = parseContent [str|\
10..9..
2...8..
3...7..
4567654
...8..3
...9..2
.....01
|]

-- started at Thu Dec 12 08:27:10 AM +04 2024
