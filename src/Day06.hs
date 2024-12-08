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
    Just guard_position = fst <$> find (\(_, elem) -> elem == '^') (Map.toList charGrid)

    toCase '.' = Free
    toCase '#' = Wall
    toCase '^' = Free
    toCase c = error $ [fmt|{c} is not a valid char for case|]
  (guard_position, fmap toCase charGrid)

-- * Generics

-- * FIRST problem
day content = length (Set.fromList $ map fst $ computePath content)

computePath (guard_position, content) = do
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
          Just Free -> go (stepCount +1) pos' dir
          Just Wall -> go stepCount pos (turnRight dir)

  go (0 :: Int) guard_position current_dir

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
day' (firstPlace, content) = length $ do
  -- First approximation of nice placments: on the initial path
  let freePlaces' = map fst $ computePath (firstPlace, content)
  let freePlaces = Set.delete firstPlace (Set.fromList freePlaces')
  freePlace <- (Set.toList freePlaces)
  let path = computePath (firstPlace, Map.insert freePlace Wall content)
  guard $ checkForInfiniteLoop path
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
