module Day25 where

import Utils
import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.Either

fileContent = parseContent $(getFile)

parseContent t = do
  let chunks = Text.splitOn "\n\n" t
  partitionEithers $ map toKeyLock chunks

toKeyLock :: Text -> Either Key Lock
toKeyLock t = do
  let grid = parse2DGrid (id @Char) t
  let (ctor, f') = if grid Map.! V2 0 0 == '.'
         then do
           -- That's a key
           (Left . Key, (6-))
         else do
           -- That's a lock
           (Right . Lock, id)
  ctor $ Map.elems $ Map.fromListWith max $ do
         (pos, item) <- Map.toList grid
         guard $ item == '#'
         pure (pos.x, f' pos.y)

data Key = Key [Int]
  deriving Show
data Lock = Lock [Int]
  deriving Show

-- * Generics

-- * FIRST problem
day (keys, locks) = length $ do
    lock <- locks
    key <- keys
    guard $ not $ overlaps lock key
    pure (lock, key)

overlaps (Lock lock) (Key key) = any (>=6) $ zipWith (+) key lock

-- * SECOND problem
day' = undefined

ex = parseContent [str|\
#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####
|]

-- started at Wed Dec 25 11:05:31 AM +04 2024
