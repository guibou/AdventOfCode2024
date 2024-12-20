module Day20 where

import Utils
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Path (shortestPath)
import Data.List
import Debug.Trace (traceShowId)

fileContent = parseContent $(getFile)

parseContent content = do
  let grid = Map.toList $ parse2DGrid (id @Char) content
  let start = head $ map fst $ filter (\(_pos, v) -> v == 'S') $ grid
  let end = head $ map fst $ filter (\(_pos, v) -> v == 'E') $ grid
  let walls = Set.fromList $ map fst $ filter (\(_pos, v) -> v == '#') $ grid
  (start, end, walls)

-- * Generics

-- * FIRST problem
countShortcuts :: (V2 Int, V2 Int, Set (V2 Int)) -> Map Int Int
countShortcuts (start, end, walls) = do
  -- This is stupid, I use a path finding algorithm to look for the NEXT point,
  -- but hey, that's already implemented.
  let Just (traceShowId -> !pathLen, path) = shortestPath
                 (\v -> do
                        dx <- drop 1 connect4
                        let v' = v + dx
                        guard $ v' `Set.notMember` walls
                        pure (1 :: Int, v')
                 )
                 (+)
                 start
                 (==end)
  let path_with_idx = zip [0..] (start:path)
  let idx_map = Map.fromList $ zip (start:path) [0..]
  let path_set = Set.fromList (start:path)

  let shortcuts = Map.fromListWith (+) $ do
        (idx0, v0) <- path_with_idx

        -- Pick a point at 2 nanosec
        v1 <- (v0+) <$> [V2 2 0, V2 (-2) 0, V2 0 2, V2 0 (-2)] 

        -- Keep point on the path
        guard $ v1 `Set.member` path_set

        case Map.lookup v1 idx_map of
          Nothing -> []
          Just idx1 -> do
            let save = idx1 - idx0 - 2
            -- Check that we are not just jumping over a non wall
            guard $ save > 0
            pure (idx1 - idx0 - 2, 1)
    
  shortcuts
  
day problem = sum $ Map.elems $ Map.filterWithKey (\k _ -> k >= 100) $ countShortcuts problem
 


-- * SECOND problem
day' = undefined

ex = parseContent [str|\
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
