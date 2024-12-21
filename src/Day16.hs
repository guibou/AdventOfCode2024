module Day16 where

import Utils
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Direction
import Path (shortestPaths, buildPath)
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as HM
import Data.Foldable (foldl')

fileContent = parseContent $(getFile)

parseContent content = do
  let grid = Map.toList $ parse2DGrid (id @Char) content
  let start = head $ map fst $ filter (\(_pos, v) -> v == 'S') $ grid
  let end = head $ map fst $ filter (\(_pos, v) -> v == 'E') $ grid
  let walls = Set.fromList $ map fst $ filter (\(_pos, v) -> v == '#') $ grid
  (start, end, walls)

-- * Generics


-- * FIRST problem
day content@(start, end, _) = fst $ fromJust $ buildPath (start, East) (\(p, _) -> p == end) $ (fmap (\(w, s) -> (w, Set.elemAt 0 s))) (findPath content)
findPath (start, end, walls) = shortestPaths
                transitionFunction
                (+)
                (start, East)
                ((\(currentPos, _) -> currentPos == end))
  where
    transitionFunction :: (V2 Int, Direction) -> [(Int, (V2 Int, Direction))]
    transitionFunction (currentPos, direction) = do
      let nextPos = currentPos + nextPosition direction
      [
       (1000, (currentPos, rotateRight direction)),
       (1000, (currentPos, rotateLeft direction))
       ] ++ if nextPos `Set.member` walls then [] else [(1, (nextPos, direction))]

nextPosition direction = case direction of
  North -> V2 0 (-1)
  South -> V2 0 (1)
  East -> V2 1 0
  West -> V2 (-1) 0

rotateRight :: Direction -> Direction
rotateRight West = North
rotateRight s = succ s

rotateLeft :: Direction -> Direction
rotateLeft North = West
rotateLeft s = pred s

-- * SECOND problem
day' content@(start, end, _) = do
  let toto = findPath content
      go :: (V2 Int, Direction) -> Set (V2 Int, Direction) -> Set (V2 Int, Direction)
      go current done
           | current `Set.member` done = done
           | current == (start, East) = Set.insert (start, East) done
           | otherwise = case HM.lookup current toto of
                    Nothing -> done
                    Just cr -> foldl' (\done next -> go next done) (Set.insert current done) (snd cr)

  let done' = Set.unions $ do
         d <- [minBound..maxBound]
         pure $ go (end, d :: Direction) mempty
  length $ Set.map fst done'

ex = parseContent [str|\
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
|]

ex' = parseContent [str|
#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
|]

-- 631 is too high!

-- started at Thu Dec 19 11:17:07 AM +04 2024
