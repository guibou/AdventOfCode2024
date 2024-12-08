module Day01 where

import Utils
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map

fileContent = parseContent $(getFile)

parseContent = unzip . fmap (\case
  [x, y] -> (x, y)
  s -> error (show s)
  ) . fmap (fmap (read @Int . Text.unpack) . Text.words) . Text.lines

-- * Generics

-- * FIRST problem
day (sort -> l0, sort -> l1) = do
  sum (zipWith (\x y -> abs (x - y)) l0 l1)

-- * SECOND problem
day' (l0, l1) = do
  let counter = Map.fromListWith (+) $ map (,1) l1
  sum $ do
    x <- l0
    case Map.lookup x counter of
      Nothing -> pure $ 0
      Just count -> pure $ x * count

ex = parseContent [fmt|\
3   4
4   3
2   5
1   3
3   9
3   3
|]
