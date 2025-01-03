module Day14 (day, day', ex, fileContent) where

import Data.Map.Strict qualified as Map
import GHC.Generics
import Utils
import qualified Data.Set as Set
import Data.List (findIndex)

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
stepRobot bounds r = r {p = mod <$> (r.p + r.v) <*> bounds}

-- * FIRST problem

stepRobots n bounds = map (applyN n  (stepRobot bounds))

day (bounds, robots) = do
  let robots' = stepRobots 100 bounds robots
  let robotsCounts = Map.fromListWith (+) $ do
        r <- robots'
        let middle = div <$> bounds <*> 2

        -- Remove the robots in the middle
        guard $ r.p.x /= middle.x && r.p.y /= middle.y
        -- Pick the quadran
        pure (V2 (if r.p.x < middle.x then 0 :: Int else 1) (if r.p.y < middle.y then 0 else 1), 1)
  product robotsCounts

-- | I'm not happy with this strategy, but it works.
--
-- I thought that the problem is generated by drawing the christmas tree, then
-- generating the random positions for the velocity and then iterating a few
-- number of time.
--
-- So by construction, the tree image do not have overlapping items.
--
-- I drawed it and it worked.
day' (bounds, robots) = findIndex (\robots' -> length (Set.fromList $ map (.p) robots') == targetLen) $ (iterate (map (stepRobot bounds)) robots)
  where
    targetLen = length robots


exBounds = V2 11 7 :: V2 Int
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
