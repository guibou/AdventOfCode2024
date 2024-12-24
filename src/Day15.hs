module Day15 where

import Utils
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics
import Direction

fileContent = parseContent $(getFile)

data World = World {
   walls :: Set (V2 Int),
   boxes :: Set (V2 Int),
   robot :: V2 Int
   }
   deriving (Show, Generic)

toDir '^' = U
toDir 'v' = D
toDir '<' = L
toDir '>' = R
toDir c = error $ show c <> " is not a direction"

parseContent t = do
  let (gridText, instructions) = Text.breakOn "\n\n" t
  let gridList = Map.toList $ parse2DGrid (id @Char) gridText

  let
    walls = Set.fromList $ map fst $ filter (\(_pos, c) -> c == '#') $ gridList
    boxes = Set.fromList $ map fst $ filter (\(_pos, c) -> c == 'O') $ gridList
    robot = case map fst $ filter (\(_pos, c) -> c == '@') $ gridList of
      [robot] -> robot
      _ -> error "Robot not found in problem"

  (World{..}, map toDir $ filter (/= '\n') $ Text.unpack $ instructions)

-- * Generics
stepWorld :: Direction -> World -> World
stepWorld inst world = do
  let delta = case inst of
                 U -> V2 0 (-1)
                 D -> V2 0 1
                 L -> V2 (-1) 0
                 R -> V2 1 0
  let nextPosition = world.robot + delta
  if nextPosition `Set.member` world.walls
  -- We cannot move, there is a wall
  then world
  else do
    if nextPosition `Set.notMember` world.boxes
    then world { robot = nextPosition}
    else do
      let lookForFreePlace currentAttempt
            | currentAttempt `Set.member` world.walls = Nothing
            | currentAttempt `Set.member` world.boxes = lookForFreePlace (currentAttempt + delta)
            | otherwise = Just currentAttempt
      -- Look for a chain of push in order to MOVE the box
      let freePlaceM = lookForFreePlace (nextPosition + delta)
      case freePlaceM of
        -- We cannot push
        Nothing -> world
        Just freePlace -> do
          -- We move the box to the free place (it is like moving all boxes by 1 step)
          world { robot = nextPosition,
                  boxes = Set.insert freePlace $ Set.delete nextPosition world.boxes
                  }

displayWorld w = display2DGrid (Map.fromSet (const "#") w.walls <> Map.fromSet (const "O") w.boxes <> Map.singleton w.robot "@")

gpsWeight (V2 x y) = 100 * y + x

-- * FIRST problem
day (world, instructions) = sum $ map gpsWeight . Set.toList . (.boxes) $ foldl' (flip stepWorld) world instructions

-- * SECOND problem
toWideWorld w = w {
     walls = Set.fromList $ concatMap (\(V2 x y) -> [V2 (2 * x) y, V2 (2 * x + 1) y]) $ Set.toList w.walls,
     robot = w.robot * V2 2 1,
     boxes = Set.map (* V2 2 1) w.boxes
}


displayWideWorld w = display2DGrid (Map.singleton w.robot "@" <> Map.fromSet (const "#") w.walls <> wideBoxesA <> wideBoxesB)
  where
    wideBoxesA = Map.fromSet (const "[") w.boxes 
    wideBoxesB = Map.fromSet (const "]") $ Set.map (\(V2 x y) -> (V2 (x + 1) y)) w.boxes 

stepWideWorld :: Direction -> World -> World
stepWideWorld inst world = do
  let delta = case inst of
                 U -> V2 0 (-1)
                 D -> V2 0 1
                 L -> V2 (-1) 0
                 R -> V2 1 0
  let nextPosition = world.robot + delta
  if nextPosition `Set.member` world.walls
  -- We cannot move, there is a wall
  then world
  else do
    -- Check that we are not on leftpart or right part of a wide box
    if not (nextPosition `Set.member` world.boxes || (nextPosition - V2 1 0) `Set.member` world.boxes)
    then world { robot = nextPosition}
    else do
      let moveBoxes [] boxes = Just boxes
          moveBoxes movedBoxes boxes = do
            let
              newBoxes = map (+delta) movedBoxes
              conflictPositions = Set.fromList $ concatMap (\(V2 x y) -> [V2 x y, V2 (x + 1) y]) newBoxes
            -- There are walls
            if not (conflictPositions `Set.disjoint` world.walls)
            then Nothing
            else do
              let
                newMovedBoxes = Set.intersection boxes (Set.fromList $ do
                  V2 x y <- Set.toList conflictPositions
                  [V2 (x - 1) y, V2 x y])
                  
                boxes' = Set.union (Set.fromList newBoxes) (Set.difference boxes newMovedBoxes)
              moveBoxes (Set.toList newMovedBoxes) boxes'
             
      let blockedBox
            | nextPosition `Set.member` world.boxes = nextPosition
            | otherwise = nextPosition - V2 1 0
      let newBoxesM = moveBoxes [blockedBox] (Set.delete blockedBox world.boxes)
      case newBoxesM of
        Nothing -> world
        Just boxes' -> world { robot = nextPosition,
                               boxes = boxes'
                             }

steps (Just n) (world, instructions) = foldl' (flip stepWideWorld) world (take n instructions)
steps Nothing (world, instructions) = foldl' (flip stepWideWorld) world instructions

day' (world, instructions) = sum $ map gpsWeight $ Set.toList $ (.boxes) $ foldl' (flip stepWideWorld) (toWideWorld world) instructions

ex = parseContent [str|\
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
|]


ex' = parseContent [str|
#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^
|]

-- started at Sun Dec 15 02:02:22 PM +04 2024
