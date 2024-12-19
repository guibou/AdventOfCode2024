module Day16 where

import Utils
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Direction
import Path (shortestPath)

fileContent = parseContent $(getFile)

parseContent content = do
  let grid = Map.toList $ parse2DGrid (id @Char) content
  let start = head $ map fst $ filter (\(_pos, v) -> v == 'S') $ grid
  let end = head $ map fst $ filter (\(_pos, v) -> v == 'E') $ grid
  let walls = Set.fromList $ map fst $ filter (\(_pos, v) -> v == '#') $ grid
  (start, end, walls)

-- * Generics


-- * FIRST problem
day (start, end, walls) = fst <$> shortestPath
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
day' = undefined

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

-- started at Thu Dec 19 11:17:07 AM +04 2024
