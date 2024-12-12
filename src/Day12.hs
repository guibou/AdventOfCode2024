{-# LANGUAGE RecordWildCards #-}
module Day12 where

import Utils
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

fileContent = parseContent $(getFile)

parseContent = parse2DGrid (id @Char)

-- * Generics

-- * FIRST problem
day m = sum $ do
  let regions = growRegions m
  map (weightRegion m) (map Set.toList $ Map.elems regions.groups')

weightRegion _m [] = 0
weightRegion m items@(s:_) = do
  let area = length items
  let currentPlant = m Map.! s
  (area * (sum $ do
    i <- items
    neightboor <- drop 1 connect4
    let i' = i + neightboor
    case Map.lookup i' m of
      Nothing -> pure 1
      Just plant'
        | plant' == currentPlant -> pure 0
        | otherwise -> pure 1))

growRegions m = go emptySuf (Map.toList m)
  where
    go suf [] = suf
    go suf ((x, plant):xs) = do
      let suf' = insertInNewGroup x suf
      let y = x + V2 1 0
      let suf'' = case Map.lookup y m of
               Nothing -> suf'
               Just plant'
                  | plant' /= plant -> suf'
                  | otherwise -> insertInSameGroup x y suf'
      let y' = x + V2 0 1
      let suf''' = case Map.lookup y' m of
               Nothing -> suf''
               Just plant'
                  | plant' /= plant -> suf''
                  | otherwise -> insertInSameGroup x y' suf''
      go suf''' xs
      
  

data SlowUnionFind x = SlowUnionFind {
   groups :: Map x Int,
   groups' :: Map Int (Set x),
   nextGroup :: Int
  }
  deriving Show

emptySuf :: Ord x => SlowUnionFind x
emptySuf = SlowUnionFind mempty mempty 0

insertInSameGroup a b suf@SlowUnionFind{..} = do
  let (groupA, groupB) = (Map.lookup a groups, Map.lookup b groups)
  case (groupA, groupB) of
    -- Items are NOT in groups
    (Nothing, Nothing) -> do
      SlowUnionFind {
        groups = Map.insert b nextGroup (Map.insert a nextGroup groups),
        nextGroup = nextGroup + 1,
        groups' = Map.insert nextGroup (Set.fromList [a, b]) groups'
      }
    (Just currentGroup, Nothing) -> 
      SlowUnionFind {
        groups = Map.insert b currentGroup groups,
        groups' = Map.insert currentGroup (Set.insert b (groups' Map.! currentGroup)) groups',
        nextGroup = nextGroup
      }
    (Nothing, Just currentGroup) -> 
      SlowUnionFind {
        groups = Map.insert a currentGroup groups,
        groups' = Map.insert currentGroup (Set.insert a (groups' Map.! currentGroup)) groups',
        nextGroup = nextGroup
      }
    (Just groupAname, Just groupBname)
      | groupAname == groupBname -> suf
      | otherwise -> do
        -- We need to merge both groups, put everything from groupB into groupA
        -- Let's pick the smallest group
        let groupA = groups' Map.! groupAname
        let groupB = groups' Map.! groupBname

        let ((smallerGroupName, smallerGroup), (biggerGroupName, _biggerGroup))
              | length groupA <= length groupB = ((groupAname, groupA), (groupBname, groupB))
              | otherwise = ((groupBname, groupB), (groupAname, groupA))

        let newGroup = Map.fromList $ do
               item <- Set.toList smallerGroup
               pure (item, biggerGroupName)
        SlowUnionFind {
           groups = newGroup <> groups,
           groups' = Map.insert biggerGroupName (groupA <> groupB) (Map.delete smallerGroupName groups'),
           nextGroup = nextGroup
         }

insertInNewGroup a suf@SlowUnionFind{..}
  | a `memberGroup` suf = suf
  | otherwise = SlowUnionFind {
     nextGroup = nextGroup + 1,
     groups = Map.insert a nextGroup groups,
     groups' = Map.insert nextGroup (Set.singleton a) groups'
  }

memberGroup a suf = Map.member a suf.groups

-- * SECOND problem
day' m = do
  let regions = growRegions m
  sum $ do
    region <- map Set.toList $ Map.elems regions.groups'
    let area = length region
    let corners = countCorners $ regionToBorderSegments (Set.fromList region)
    -- let sides = sum $ map snd $ countSides (Set.fromList region)
    pure (area * corners)
    
--- From a region, returns the list of the border segments
regionToBorderSegments :: Set (V2 Int) -> [(V2 Int, V2 Int)]
regionToBorderSegments plants = map fst $ filter (\(_pos, count) -> count == 1) $ Map.toList $ Map.fromListWith (+) $ map (, 1 :: Int) $ do
  plant <- Set.toList plants
  -- Generate the 4 corners around the plant
  let tl = plant
      tr = plant + V2 1 0
      bl = plant + V2 0 1
      br = bl + V2 1 0
  -- Generates the 4 segments around the plant
  -- Note the order, left to right, top to bottom, to ensure correct merge
  [ (tl, tr), -- Top
    (tl, bl), -- Left
    (bl, br), -- Bottom
    (tr, br) -- Right
   ]

countCorners :: [(V2 Int, V2 Int)] -> Int
countCorners edges = do
  let pairedEdges = Map.fromListWith (++) $ do
              (p0, p1) <- edges
              [(p0, [p1]), (p1, [p0])]
  sum $ map isCorner $ Map.toList $ pairedEdges
  -- pairedEdges

isCorner :: (V2 Int, [V2 Int]) -> Int
isCorner (centerPoint, [a, b]) = do
  let
    da = a - centerPoint
    db = b - centerPoint
  if da /= -db then 1 else 0
-- Special case:
--    |
--   -o-
--    |
--
--  That's always 2 corners
isCorner (_centerPoint, _) = 2

red t = "\ESC[31;1;4m" <> t <> "\ESC[0m"
redC c = red (Text.singleton c)

ex = parseContent [str|\
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
|]

ex' = parseContent [str|\
OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
|]

ex'' = parseContent [str|\
AAAA
BBCD
BBCC
EEEC
|]

-- started at Thu Dec 12 09:14:58 AM +04 2024
