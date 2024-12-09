{-# LANGUAGE RecordWildCards #-}
module Day09 where

import Utils
import qualified Data.Text as Text

fileContent = parseContent $(getFile)

parseContent = Text.unpack . Text.strip

-- * Generics

-- * FIRST problem
day input = sum $ zipWith (*) [0..] (streamIdx (compact (toBlock 0 input)))

toBlock :: Int -> [Char] -> [Block]
toBlock _ [] = []
toBlock idx [x] = [Block (read [x]) (Data idx)]
toBlock !idx (x:y:xs) = bData: (freeOp (toBlock (idx+1) xs))
  where
    bData = Block (read [x]) (Data idx)
    freeSize = read [y]
    freeOp
      | freeSize == 0 = id
      | otherwise = ((Block freeSize Free):)

data Block = Block !Int Kind
  deriving Show
data Kind = Free | Data !Int
  deriving Show

data CompactedBlock = CompactedBlock { size :: Int, idx :: Int }
  deriving (Show)

getDataSize (Block size Free) = 0
getDataSize (Block size (Data _)) = size

compact :: [Block] -> [CompactedBlock]
compact input = takeContinuousBlocks l $ go input reversed
  where
    l = sum $ map getDataSize input
    reversed = reverse input

    -- Purge free on queue
    go xl (Block _size Free:ys) = go xl ys
    -- Progress on head
    go ((Block size (Data idx)):xl) yl = CompactedBlock{..}:go xl yl

    go (Block 0 Free:xl) yl = go xl yl
    go xl (Block 0 (Data _):yl) = go xl yl

    go (Block n Free:xl) (Block n' (Data idx):yl) = CompactedBlock { size = 1, idx=idx}:go (Block (n-1) Free: xl) (Block (n' - 1) (Data idx):yl)
       

takeContinuousBlocks l ((CompactedBlock size idx):xs)
  | l >= size = CompactedBlock size idx:takeContinuousBlocks (l - size) xs
  | otherwise = [CompactedBlock l idx]

streamIdx [] = []
streamIdx ((CompactedBlock size idx):xs) = replicate size idx <> streamIdx xs

streamIdx' [] = []
streamIdx' ((Block size (Data idx)):xs) = replicate size idx <> streamIdx' xs
streamIdx' ((Block size Free):xs) = replicate size 0 <> streamIdx' xs

-- * SECOND problem
day' input = sum $ zipWith (*) [0..] (streamIdx' (compact' (toBlock 0 input)))

compact' :: [Block] -> [Block]
compact' input = go input reversed
  where
    l = sum $ map getDataSize input
    reversed = reverse input

    -- We tried all the blocks
    go current [] = current

    -- Do not try to reinsert Free
    go current ((Block _size Free):ys) = go current ys
    go current (b@(Block _size (Data _idx)):ys) = go (reinsert b current) ys

    reinsert (Block _size Free) _current = error "YOU SHOULD NOT REINSERT FREE BLOCK"
    reinsert (Block size (Data idx)) current = go current
      where
        go [] = [] 
        go (b@(Block size' Free):ys)
          -- We cannot reinsert here
          | size' < size = b:go ys
          -- We can reinsert here
          | otherwise = (Block size (Data idx)):Block (size' - size) Free:removeBlock idx ys
        go (b@(Block size' (Data idx')):ys)
          -- Found the same block, we can stop here, it was not reinserted
          | idx' == idx = b:ys
          -- Skipping this block
          | otherwise = b:go ys

removeBlock  idx [] = []
removeBlock  idx (b@(Block size Free):ys) = b:removeBlock idx ys
removeBlock  idx (b@(Block size (Data idx')):ys)
  | idx == idx' = Block size Free:ys
  | otherwise = b:removeBlock idx ys


ex = "2333133121414131402"
