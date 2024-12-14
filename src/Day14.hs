module Day14 where

import Utils
import GHC.Generics
import Control.Applicative (some)
import qualified Data.Map.Strict as Map

data Robot = Robot {
  p :: V2 Int,
  v :: V2 Int
 }
 deriving (Show, Generic)

fileContent = parseContent $(getFile)

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
stepRobot bounds r = r { p = (r.p + r.v) `modV2` bounds }

(V2 a b) `modV2` (V2 x y) = V2 (a `mod` x) (b `mod` y)
(V2 a b) `divV2` (V2 x y) = V2 (a `div` x) (b `div` y)

-- * FIRST problem
fileBounds = V2 101 103 :: V2 Int
exBounds = V2 11 7 :: V2 Int

stepRobots bounds = map (applyN 100 (stepRobot bounds))

day robots bounds = do
  let robots' = map (applyN 100 (stepRobot bounds)) robots
  let robotsCounts = Map.fromListWith (+) $ do
        r <- robots'
        let middle = bounds `divV2` 2

        -- Remove the robots in the middle
        guard $ r.p.x /= middle.x && r.p.y /= middle.y
        -- Pick the quadran
        pure (V2 (if r.p.x < middle.x then 0 else 1) (if r.p.y < middle.y then 0 else 1), 1)
  product robotsCounts

-- * SECOND problem
day' = undefined

ex = parseContent [str|\
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

-- started at Sat Dec 14 03:36:15 PM +04 2024
