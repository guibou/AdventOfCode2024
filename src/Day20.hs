module Day20 where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Path (shortestPath)
import Utils

fileContent = parseContent $(getFile)

parseContent content = do
  let grid = Map.toList $ parse2DGrid (id @Char) content
  let start = head $ map fst $ filter (\(_pos, v) -> v == 'S') $ grid
  let end = head $ map fst $ filter (\(_pos, v) -> v == 'E') $ grid
  let walls = Set.fromList $ map fst $ filter (\(_pos, v) -> v == '#') $ grid
  (start, end, walls)

-- * Generics

countShortcuts ::
  -- | Maximum size of the hop
  Int ->
  -- | (start, end, walls)
  (V2 Int, V2 Int, Set (V2 Int)) ->
  Map Int Int
countShortcuts jumpSize (start, end, walls) = do
  -- This is stupid, I use a path finding algorithm to look for the NEXT point,
  -- but hey, that's already implemented.
  let path =
        case shortestPath
          ( \v -> do
              dx <- drop 1 connect4
              let v' = v + dx
              guard $ v' `Set.notMember` walls
              pure (1 :: Int, v')
          )
          (+)
          start
          (== end) of
            Nothing -> error "Cannot find a path!"
            Just (_, path) -> path
  let path_with_idx = zip [0 ..] (start : path)
  let idx_map = Map.fromList $ zip (start : path) [0 ..]
  let path_set = Set.fromList (start : path)

  let neighboorhood = buildManahathanNeighooborooud jumpSize

  let shortcuts = Map.fromListWith (+) $ do
        -- O(n)
        (idx0, v0) <- path_with_idx

        -- Pick the second point, at the correct manhanthan distance and ensured to be in the path_set
        -- O(jumpSize ^ 2)
        let vs' = Set.intersection (Set.map (+ v0) neighboorhood) path_set
        v1 <- Set.toList vs'

        case Map.lookup v1 idx_map of
          Nothing -> []
          Just idx1 -> do
            let cheatDistance = manDistance v0 v1
            let save = idx1 - idx0 - cheatDistance

            pure (save, 1)

  shortcuts

manDistance (V2 x y) (V2 x' y') = abs (x - x') + abs (y - y')

buildManahathanNeighooborooud distance = Set.fromList $ do
  x <- [0 .. distance]
  y <- [0 .. (distance - x)]

  [V2 x y, V2 (-x) y, V2 x (-y), V2 (-x) (-y)]

-- * FIRST problem

day limit problem = sum $ Map.elems $ Map.filterWithKey (\k _ -> k >= limit) $ countShortcuts 2 problem

-- * SECOND problem

day' limit problem = sum $ Map.elems $ Map.filterWithKey (\k _ -> k >= limit) $ countShortcuts 20 problem

ex =
  parseContent
    [str|\
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
|]

-- started at Fri Dec 20 10:10:13 PM +04 2024
