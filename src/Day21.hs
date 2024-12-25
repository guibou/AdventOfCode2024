module Day21 where

import Utils
import qualified Data.Text as Text
import GHC.Stack
import Data.List (nub)

fileContent = parseContent $(getFile)

parseContent = lines . Text.unpack

-- * Generics
elemString :: Char -> String -> Bool
elemString = elem

-- * keypad
keypadLine :: Char -> Int
keypadLine x
  | x `elemString` "0A" = 0
  | x `elemString` "123" = 1
  | x `elemString` "456" = 2
  | x `elemString` "789" = 3
  | otherwise = error $ show x

keypadColumn :: Char -> Int
keypadColumn x
  | x `elemString` "147" = 0
  | x `elemString` "8520" = 1
  | x `elemString` "963A" = 2
  | otherwise = error $ show x

keypadToPos :: Char -> V2 Int
keypadToPos c = V2 (keypadColumn c) (keypadLine c)

keypadMove from to = do
      let posFrom = keypadToPos from
          posTo = keypadToPos to
          deltaPos = posTo - posFrom

          verticalMoves = replicate (abs deltaPos.y) (toVerticalMove $ signum deltaPos.y)
          horizontalMoves = replicate (abs deltaPos.x) (toHorizontalMove $ signum deltaPos.x)

      -- Handle the geometry of the keypad to ensure to falling in the hole
      if
        | from `elemString` "0A" && to `elemString` "741" -> [verticalMoves <> horizontalMoves]
        | from `elemString` "741" && to `elemString` "0A"  -> [horizontalMoves <> verticalMoves]
        | otherwise -> nub [verticalMoves <> horizontalMoves, horizontalMoves <> verticalMoves]

toVerticalMove :: HasCallStack => Int -> Char
toVerticalMove 1 = '^'
toVerticalMove (-1) = 'v'
toVerticalMove x = error $ show x

toHorizontalMove :: HasCallStack => Int -> Char
toHorizontalMove 1 = '>'
toHorizontalMove (-1) = '<'
toHorizontalMove x = error $ show x

-- Assumes the code starts on "A" and ends on "A"
enterCode startPos code = go startPos code
  where
    go _ [] = [[]]
    go from (to:xs) = do
      moves <- keypadMove from to
      nexts <- go to xs

      pure $ moves <> "A" <> nexts

-- controller
controllerLine :: Char -> Int
controllerLine x
  | x `elemString` "^A" = 1
  | x `elemString` "<v>" = 0
  | otherwise = error $ show x

controllerColumn :: Char -> Int
controllerColumn x
  | x `elemString` "<" = 0
  | x `elemString` "^v" = 1
  | x `elemString` "A>" = 2
  | otherwise = error $ show x

-- Maybe it could be generalized ;)
controllerToPos :: Char -> V2 Int
controllerToPos c = V2 (controllerColumn c) (controllerLine c)

controllerMove from to = do
      let posFrom = controllerToPos from
          posTo = controllerToPos to
          deltaPos = posTo - posFrom

          verticalMoves = replicate (abs deltaPos.y) (toVerticalMove $ signum deltaPos.y)
          horizontalMoves = replicate (abs deltaPos.x) (toHorizontalMove $ signum deltaPos.x)

      -- Handle the geometry of the controller to ensure not falling in the hole
      if
        | from `elemString` "^A" && to `elemString` "<" -> [verticalMoves <> horizontalMoves]
        | from `elemString` "<" && to `elemString` "^A" -> [horizontalMoves <> verticalMoves]
        | otherwise -> nub [horizontalMoves <> verticalMoves, verticalMoves <> horizontalMoves]

enterMoves :: Int -> [Char] -> Int
enterMoves 0 moves = length moves
enterMoves n moves  = go n moves
  where
    f n from to = minimum $ do
            move <- controllerMove from to
            pure $ go (n -1) (move <> "A")

    fMemo = memoize3 f

    go 0 moves = length moves

    go n moves = do
      let 
        from_to = zip ('A':moves) moves
        next_moves = sum $ map (\(from, to) -> fMemo n from to) from_to
      next_moves

-- * FIRST problem
day = run 2

getSequence :: Int -> String -> Int
getSequence n code = minimum $ do
  a <- enterCode 'A' code
  pure $ enterMoves n a

weightCode (n :: Int) code = do
  let lenButtonPress = getSequence n code
  let weight = lenButtonPress * read (take 3 code)
  weight

run n = sum . map (weightCode n)

-- * SECOND problem
-- Brute force DOES NOT work
day' = run 25

ex = parseContent [str|\
029A
980A
179A
456A
379A
|]


-- 112958 is too high! ?
-- 111474 is too high too...
--
-- 153242207901942 is too high!
--
-- started at Sun Dec 22 03:02:06 PM +04 2024
