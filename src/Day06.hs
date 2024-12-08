module Day06 where

import Utils
import qualified Data.Map.Strict as Map
import Data.List (find)
import Direction
import Debug.Trace
import qualified Data.Set as Set
import Control.Monad (guard)

fileContent = parseContent $(getFile)

parseContent = parse2DGrid (id @Char)

-- * Generics

-- * FIRST problem
day content = length (Set.fromList $ map fst $ computePath content)

computePath content = do
  let Just guard_position = fst <$> find (\(_, elem) -> elem == '^') (Map.toList content)
  let current_dir = U

  let go !stepCount pos dir = (pos, dir): do
        let 
          pos' = case dir of
             U -> pos + V2 0 (-1)
             D -> pos + V2 0 1
             L -> pos + V2 (-1) 0
             R -> pos + V2 1 0
        case Map.lookup pos' content of
          Nothing -> []
          Just '.' -> go (stepCount +1) pos' dir
          Just '#' -> go stepCount pos (turnRight dir)
          Just '^' -> go (stepCount + 1) pos' dir

  go 0 guard_position current_dir

turnRight U = R
turnRight R = D
turnRight D = L
turnRight L = U

checkForInfiniteLoop l = go mempty l
  where 
    go _ [] = False
    go known (x:xs)
      | x `Set.member` known = True
      | otherwise = go (Set.insert x known) xs

-- * SECOND problem
day' content = length $ do
  -- First approximation of nice placments: on the initial path
  -- let freePlaces = Map.keys $ Map.filter (\x -> x == '.') content
  let (firstPlace:freePlaces') = map fst $ computePath content
  let freePlaces = Set.delete firstPlace (Set.fromList freePlaces')
  (freeIdx, freePlace) <- zip [0..] (Set.toList freePlaces)
  let path = computePath (Map.insert freePlace '#' content)
  guard $ traceShow ("checking", freeIdx, length freePlaces) $ checkForInfiniteLoop path
  pure freePlace

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
