module Day06 where

import Utils
import qualified Data.Map.Strict as Map
import Data.List (find)
import Direction
import qualified Data.Set as Set

fileContent = parseContent $(getFile)

data Case = Free | Wall

parseContent content = do
  let
    charGrid = parse2DGrid (id @Char) content
    guard_position = case fst <$> find (\(_, elem) -> elem == '^') (Map.toList charGrid) of
       Nothing -> error "No guard position in the input data"
       Just v -> v

    toCase '.' = Free
    toCase '#' = Wall
    toCase '^' = Free
    toCase c = error $ [fmt|{c} is not a valid char for case|]
  (guard_position, fmap toCase charGrid)

-- * Generics

-- * FIRST problem
day content = length (Set.fromList $ map fst $ computePath content)

computePath (guard_position, content) = walkPath content guard_position U


walkPath content pos dir = (pos, dir): nextStep pos dir
  where
    nextStep pos dir = do
        let 
          pos' = pos + case dir of
             U -> V2 0 (-1)
             D -> V2 0 1
             L -> V2 (-1) 0
             R -> V2 1 0
        case Map.lookup pos' content of
          Nothing -> []
          Just Free -> walkPath content pos' dir
          Just Wall -> nextStep pos (turnRight dir)

turnRight U = R
turnRight R = D
turnRight D = L
turnRight L = U

checkForInfiniteLoop l = go l (drop 1 l)
  where 
    go (x:xs) (y:_skipped:ys) = if x == y then True else go xs ys
    go _ _ = False

-- * SECOND problem
day' (firstPlace, content) = do
  -- First approximation of obstacle positions: on the initial path
  let firstPath = computePath (firstPlace, content)

  -- We will then walk this path and add obstacles on it on the next step
  go 0 mempty firstPath
  where
      go :: Int -> Set (V2 Int) -> [(V2 Int, Direction)] -> Int
      go !acc seenObstacle ((currentPos, currentDir):toto@((nextPos, _nextDir):_))
        -- We won't add an object on the next position if it was the starting one
        -- So this is the normal path, so not a loop, so no increase of the acc
        | nextPos == firstPlace = go acc seenObstacle toto
        -- DO NOT insert an abstacle on a place which was already used for obstacle
        | nextPos `Set.member` seenObstacle = go acc seenObstacle toto
        | otherwise = do
           -- Here we can insert the obstacle
           let m = Map.insert nextPos Wall content
           let newPath = walkPath m currentPos (turnRight currentDir)
           go (if checkForInfiniteLoop newPath then acc + 1 else acc) (Set.insert nextPos seenObstacle) toto
      go acc _ [_] = acc
      go !_acc _ [] = error "Should not happen"

ex = parseContent [str|\
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
|]

-- started at Sun Dec  8 14:24:14 PM +04 2024
