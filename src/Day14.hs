module Day14 where

import Control.Applicative (some)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import GHC.Generics
import Utils
import Control.Monad (when)
import qualified Data.Set as Set
import Data.List (find)
import Debug.Trace

data Robot = Robot
  { p :: V2 Int,
    v :: V2 Int
  }
  deriving (Show, Generic)

fileBounds = V2 101 103 :: V2 Int

fileContent = (fileBounds, parseContent $(getFile))

parseContent = unsafeParse $ some parseRobot

parseRobot = do
  void "p="
  px <- parseNumber
  void ","
  py <- parseNumber
  void "v="
  vx <- parseNumber
  void ","
  vy <- parseNumber
  void "\n"
  pure $ Robot {p = V2 px py, v = V2 vx vy}

-- * Generics

stepRobot :: V2 Int -> Robot -> Robot
-- Note: this could be done instantly by multipling the speed by N
stepRobot bounds r = r {p = (r.p + r.v) `modV2` bounds}

(V2 a b) `modV2` (V2 x y) = V2 (a `mod` x) (b `mod` y)

(V2 a b) `divV2` (V2 x y) = V2 (a `div` x) (b `div` y)

-- * FIRST problem

stepRobots n bounds = map (applyN n  (stepRobot bounds))

day (bounds, robots) = do
  let robots' = stepRobots 100 bounds robots
  let robotsCounts = Map.fromListWith (+) $ do
        r <- robots'
        let middle = bounds `divV2` 2

        -- Remove the robots in the middle
        guard $ r.p.x /= middle.x && r.p.y /= middle.y
        -- Pick the quadran
        pure (V2 (if r.p.x < middle.x then 0 else 1) (if r.p.y < middle.y then 0 else 1), 1)
  product robotsCounts

robotCounts (bounds, robots) = Map.fromListWith (+) $ do
  r <- robots
  pure (r.p, 1)

-- * SECOND problem

day' = undefined

displayRobots (bounds, robots') steps = do
  let robots = stepRobots steps bounds robots'
  let boundsMap = Map.fromList $ do
        y <- [0 .. bounds.y]
        x <- [0 .. bounds.x]

        pure (V2 x y, " ")
  display2DGrid (fmap (Text.pack . show) (robotCounts (bounds, robots)) <> boundsMap)

displayRobots' (bounds, robots) = do
  let boundsMap = Map.fromList $ do
        y <- [0 .. bounds.y]
        x <- [0 .. bounds.x]

        pure (V2 x y, ".")

  let go i robots = do
        putStrLn $ "--- " <> show i
        when (checkConnexity robots) $ do
          display2DGrid (fmap (Text.pack . show) (robotCounts (bounds, robots)) <> boundsMap)
          _ <- getLine
          pure ()
        go (i + 1) (stepRobots 1 bounds robots)
  go 0 robots

findEsterEgg (bounds, robots) = do
  let allRobots = iterate (map (stepRobot bounds)) robots
      treeSet = buildChritmasTree bounds
  traceShow (length treeSet, length robots) $ find (\(i, robots) -> Set.fromList (map (\r -> r.p) robots) == treeSet) $ zip [0..] allRobots

exBounds = V2 11 7 :: V2 Int

buildChritmasTree :: V2 Int -> Set (V2 Int)
buildChritmasTree bounds = Set.fromList $ do
          y <- [0 .. bounds.y - 1]
          x <- [bounds.x `div` 2 - y, bounds.x `div` 2 + y]
          pure $ V2 x y

checkConnexity robots = all hasFriend robots
  where
    robotsPos = Set.fromList $ map (.p) robots
    hasFriend r = any (\p -> p `Set.member` robotsPos) $ (map (r.p+) (drop 1 connect8))

ex =
  ( exBounds,
    parseContent
      [str|\
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
|]
  )

-- started at Sat Dec 14 03:36:15 PM +04 2024
