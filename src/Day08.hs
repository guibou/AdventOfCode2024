module Day08 where

import Utils
import Control.Monad
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

fileContent = parseContent $(getFile)

parseContent = parse2DGrid (id @Char)

-- * Generics

-- * FIRST problem
day content = length $ do
  let antennas = Map.fromListWith (++) $ do
         (pos, antenna) <- Map.toList content
         guard $ antenna /= '.'
         pure (antenna, [pos])
  Set.filter (\p -> Map.member p content) $ Set.fromList $ concatMap buildAntiNodes (Map.toList antennas)

buildAntiNodes :: (Char, [V2 Int]) -> [V2 Int]
buildAntiNodes (name, positions) = do
  (a, a') <- buildPairs positions
  -- compute the position of both antinodes
  let delta = a' - a
  [a - delta, a' + delta]

buildPairs :: [a] -> [(a, a)]
buildPairs [] = []
buildPairs (x:xs) = do
  (fmap (\y -> (x, y)) xs) ++ buildPairs xs

-- * SECOND problem
day' content = length $ do
  let antennas = Map.fromListWith (++) $ do
         (pos, antenna) <- Map.toList content
         guard $ antenna /= '.'
         pure (antenna, [pos])
  Set.filter (\p -> Map.member p content) $ Set.fromList $ concatMap (buildAntiNodes' content) (Map.toList antennas)

buildAntiNodes' :: _ -> (Char, [V2 Int]) -> [V2 Int]
buildAntiNodes' content (name, positions) = do
  (a, a') <- buildPairs positions
  -- compute the position of all antinodes by just walking in both direction indefinitely
  let delta = a' - a
  let map1 = takeWhile (\pos -> Map.member pos content) $ iterate (+delta) a
  let map2 = takeWhile (\pos -> Map.member pos content) $ iterate (subtract delta) a
  map1 <> map2



ex = 
  parseContent [str|\
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
|]

ex' = parseContent [str|\
..........
..........
..........
....a.....
..........
.....a....
..........
..........
..........
..........
|]

-- started at Sun Dec  8 15:06:08 PM +04 2024
