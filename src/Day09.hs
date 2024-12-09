{-# LANGUAGE RecordWildCards #-}
module Day09 where

import Utils
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (minimumBy)
import Data.Ord (comparing)

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

getDataSize (Block _size Free) = 0
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
    go _ _ = error "These cases are not used"
       

takeContinuousBlocks l ((CompactedBlock size idx):xs)
  | l >= size = CompactedBlock size idx:takeContinuousBlocks (l - size) xs
  | otherwise = [CompactedBlock l idx]
takeContinuousBlocks _ [] = error "This list is supposed to be infinite"

streamIdx [] = []
streamIdx ((CompactedBlock size idx):xs) = replicate size idx <> streamIdx xs

streamIdx' [] = []
streamIdx' ((Block size (Data idx)):xs) = replicate size idx <> streamIdx' xs
streamIdx' ((Block size Free):xs) = replicate size 0 <> streamIdx' xs

-- * SECOND problem
day' input = sum $ map weight $ Map.toList (compact' (toBlock 0 input))

weight :: (Int, (Int, Int)) -> Int
weight (offset, (size, idx)) = idx * sum [offset..offset + size - 1]

toOffseted :: [Block] -> [(Int, Block)]
toOffseted blocks = go 0 blocks
  where
    go !offset v = case v of
         [] -> []
         b@(Block size _):vs -> (offset, b):go (offset + size) vs

compact' :: [Block] -> (Map Int (Int, Int))
compact' input = go dataBlocks freeBlocks blocks_to_reinsert
  where
    offseted = toOffseted input
    dataBlocks = Map.fromList $ do
      (offset, block) <- offseted
      case block of
        Block _size Free -> []
        Block size (Data dt) -> [(offset, (size, dt))]

    freeBlocks = fmap Set.fromList $ Map.fromListWith (++) $ do
      (offset, block) <- offseted
      case block of
        Block size Free -> [(size, [offset])]
        Block _size (Data _dt) -> []

    blocks_to_reinsert = reverse $ Map.toList dataBlocks

    go :: Map Int (Int, Int) -> Map Int (Set Int) -> [(Int, (Int, Int))] -> Map Int (Int, Int)
    go !dataBlocks _freeBlocks [] = dataBlocks
    go !dataBlocks (Map.filter (not . null) -> freeBlocks) ((currentOffset, (size, dt)):xs) = do
      -- Find a freeblock which size is bigger
      let freeBlockSizes = filter (>= size) $ Map.keys freeBlocks
      case freeBlockSizes of
        -- There is no available free block for this
        [] -> go dataBlocks freeBlocks xs
        _ -> do
           let closerFreeBlockSize = minimumBy (comparing (\k -> (Set.elemAt 0 (freeBlocks Map.! k)))) freeBlockSizes
           let freeBlockOffset = Set.elemAt 0 (freeBlocks Map.! closerFreeBlockSize)

           if freeBlockOffset >= currentOffset
           -- We fall in the SAME place, or after, that's not a good idea
           then go dataBlocks freeBlocks xs
           else do
             -- We need to remove the freeBlock from the list of freeBlocks
             let freeBlocks' = Map.insert closerFreeBlockSize (Set.drop 1 $ freeBlocks Map.! closerFreeBlockSize) freeBlocks
             -- We remove the block from the previous position and insert it an the next position
             let dataBlocks' = Map.insert freeBlockOffset (size, dt) (Map.delete currentOffset dataBlocks)
             -- We need to reinsert the freeBlock IF not null
                 freeBlocks'' = if closerFreeBlockSize == size
                                then freeBlocks'
                                else do
                                   let newFreeSize = closerFreeBlockSize - size
                                   let newFreeOffset = freeBlockOffset + size
                                   Map.insert newFreeSize (Set.insert newFreeOffset (freeBlocks' Map.! newFreeSize)) freeBlocks'


             -- We need to 
             go dataBlocks' freeBlocks'' xs



ex :: String
ex = "2333133121414131402"
