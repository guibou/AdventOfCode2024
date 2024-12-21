module Day18 where

import Utils
import Path (shortestPath)
import qualified Data.Set as Set

fileContent = (V2 (70 :: Int) 70, parseContent $(getFile))

parseContent = unsafeParse $ do
  (some (V2 <$> parseNumber @Int <*> ("," *> parseNumber <* "\n")))

-- * Generics

-- * FIRST problem
day bounds problem = fst <$> findPath bounds problem

findPath simulated (bounds, corrupted) = shortestPath @_ @Int
   transitionFunction
   (+)
   (V2 0 0)
   (==bounds)
   where
     corruptedSet = Set.fromList $ take simulated corrupted
     transitionFunction currentPos = do
       dp <- drop 1 connect4
       let p' = currentPos + dp
       guard $ p' `Set.notMember` corruptedSet
       guard $ p'.x >= 0 && p'.y >= 0 && p'.x <= bounds.x && p'.y <= bounds.y
       pure (1, p')
          

-- * SECOND problem
day' (bounds, corrupted) = go 0 (length corrupted)
  where
    go minBound maxBound
      | minBound + 1 == maxBound = (corrupted !! minBound)
      | otherwise = do
         let currentBound = (minBound + maxBound) `div` 2
         if findPath currentBound (bounds, corrupted) == Nothing
         then go minBound currentBound
         else go currentBound maxBound

ex = (V2 (6 :: Int) 6, parseContent [str|\
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
|])

-- started at Thu Dec 19 12:08:22 PM +04 2024
