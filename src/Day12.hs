{-# LANGUAGE RecordWildCards #-}
module Day12 where

import Utils
import qualified Data.Set as Set
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Debug.Trace

fileContent = parseContent $(getFile)

parseContent = parse2DGrid (id @Char)

-- * Generics

-- * FIRST problem
day m = sum $ do
  let regions = growRegions m
  map (weightRegion m) (map Set.toList $ Map.elems regions.groups')

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

debug m = do
  let regions = (growRegions m).groups'
  let summary = fmap (\x -> (length x, sum x)) $ Map.fromListWith (<>) $ do
        (Set.toList -> items) <- Map.elems regions
        pure (m Map.! head items, [length items])
  summary

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
    let sides = sum $ map snd $ countSides (Set.fromList region)
    pure (area * sides)
    

finish :: [(Int, [(a, Int)])] -> Int
finish = sum . map (\(x, y) -> x * sum (map snd y))

getBorder m items@(s:_) = do
  let area = length items
  let currentPlant = m Map.! s
  let border = do
        i <- items
        neightboor <- drop 1 connect8
        let i' = i + neightboor
        case Map.lookup i' m of
          Nothing -> pure i'
          Just plant'
            | plant' == currentPlant -> []
            | otherwise -> pure i'
  Set.fromList $ border

partitionConnected :: Set (V2 Int) -> _
partitionConnected elems = go emptySuf (Set.toList elems)
  where
    go suf [] = suf
    go suf (x:xs) = do
      let neightboors = filter (`Set.member` elems) $ map (x+) $ drop 1 $ connect4
      if null neightboors
      then go (insertInNewGroup x suf) xs
      else go (foldl' (\suf x' -> insertInSameGroup x x' suf) suf neightboors) xs

countSides :: Set (V2 Int) -> [(V2 Int, Int)]
countSides itemsSet
  | length itemsSet == 1 = [(head $ Set.toList itemsSet, 4 :: Int)]
  | otherwise = do
   i <- Set.toList itemsSet
   pure $ (countCorners itemsSet i)

countCorners itemSet item = (item, ) $ do
  let possibleNeighbors = filter (\n -> Set.member n itemSet) $ map (\d -> item + d) $ drop 1 connect4
  case possibleNeighbors of
    [a, b] -> do
      let d = abs (a - b)
      case d of
        V2 0 2 -> 0
        V2 2 0 -> 0
        _ -> 1
    [x] -> 2
    [a, b, c] -> traceShow "YOTO" 2
    what -> error (show (what, item, itemSet))

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
--
-- 1461802 is too low!
--
--
{-
 -
 - That one has 1 corner
 -    *-*
      | |
      | +_
      |

       Y
       +X
       Z
Entrance and exit are not

 RRRRIICCFF
....RIICCCF
.VV.RRCCFFF
.VV...CJFFF
.VVVV.JJCFE
.VV.V.CJJEE
.VV...CJJEE
....IIIJJEE
 MIIISIJEEE
 MMMISSJEEE

RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VV.VCCJJEE
VVI.ICJJEE
MI.IIIJJEE
MIIISIJEEE
MMMISSJEEE


that one has two

    |
  *-+
  *-+
    |

    X
   Yo
    Z

     1001
     0..11
    11...0
  101..101
  0...11
  11.22.
   0..0
   11.0
    101
    

XX
Xb
XX
entrance and exit are on the same path

-}
{-
([],V2 16 31,fromList [V2 (-1) 22,V2 (-1) 23,V2 (-1) 24,V2 (-1) 25,V2 (-1) 26,V2 (-1) 27,V2 (-1) 28,V2 (-1) 29,V2 0 22,V2 0 29,V2 0 30,V2 1 22,V2 1 30,V2 2 22,V2 2 29,V2 2 30,V2
 3 22,V2 3 30,V2 3 31,V2 4 22,V2 4 31,V2 4 32,V2 4 33,V2 4 34,V2 5 22,V2 5 23,V2 5 24,V2 5 34,V2 5 35,V2 6 23,V2 6 26,V2 6 27,V2 6 35,V2 7 23,V2 7 24,V2 7 25,V2 7 26,V2 7 27,V2 7 28,V2 7 29,V2
 7 30,V2 7 35,V2 8 29,V2 8 30,V2 8 35,V2 9 29,V2 9 35,V2 10 28,V2 10 29,V2 10 30,V2 10 35,V2 11 28,V2 11 30,V2 11 35,V2 12 26,V2 12 27,V2 12 28,V2 12 34,V2 12 35,V2 13 26,V2 13 35,V2 13 36,V2 
14 26,V2 14 27,V2 14 28,V2 14 36,V2 15 28,V2 15 35,V2 15 36,V2 16 28,V2 16 31,V2 16 34,V2 16 35,V2 17 28,V2 17 34,V2 18 28,V2 18 33,V2 18 34,V2 19 28,V2 19 29,V2 19 32,V2 19 33,V2 20 29,V2 20 
30,V2 20 31,V2 20 32])
-}


{-

Patterns of adjacency cells:

XXX
ooo  -
XXX

-}


