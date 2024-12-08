{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}

module Utils
  ( module Utils,
    module Relude.Extra,
    Vector,
    module Data.Function.Memoize,
    module Linear,
    here,
    hereLit,
    chunksOf,
    genum,
    GEnum,
    module PyF,
    Set,
    Map,
    Text,
    module Data.List,
    Hashable (..),
    NFData (..),
    module Data.Void,
    module Data.Functor.Identity,
    ($>),
    (<$),

    -- * Nice reexport
    void, guard
  )
where

import Bezout
import Control.DeepSeq (NFData (..))
import Control.Monad.Memo
import Control.Parallel.Strategies (parBuffer, rdeepseq, using)
import Crypto.Hash.MD5 qualified
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (encode)
import Data.Char (toLower)
import Data.FileEmbed (embedFile)
import Data.Function.Memoize
import Data.Functor (($>))
import Data.Functor.Identity
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable (..))
import Data.List (isPrefixOf, sort)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Here
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void
import Generics.Deriving.Enum (GEnum, genum)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Linear hiding (ex, trace, transpose)
import PyF
import Relude.Extra
import Relude.Unsafe qualified as Unsafe
import Safe (readMay)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import GHC.Stack

-- So I can use it in the shell
-- dayX <$$> content

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = (fmap . fmap)

infixl 4 <$$>

countItem :: (Eq a) => a -> [a] -> Int
countItem x = countIf (== x)

countIf :: (a -> Bool) -> [a] -> Int
countIf p l = length (filter p l)

bfs :: (Ord p) => (Set p -> Set p -> Int -> Bool) -> p -> (p -> [p]) -> (Set p, Set p, Int)
bfs stopCriterion start stepFunction = go (Set.singleton start) Set.empty 0
  where
    go todos visited depth
      | stopCriterion todos visited depth = (todos, visited, depth)
      | otherwise =
          let newSteps = Set.fromList (mconcat (map stepFunction (Set.toList todos)))
              okSteps = Set.difference newSteps visited
           in go okSteps (Set.union todos visited) (depth + 1)

md5 :: ByteString -> ByteString
md5 = encode . Crypto.Hash.MD5.hash

parBufferChunks :: (NFData t) => [t] -> [t]
parBufferChunks l =
  let chunks = chunksOf 4096 l
   in mconcat chunks `using` parBuffer 20 rdeepseq

--
-- TODO: do not fail if the file does not exists
-- (only warn)
getFile :: Q Exp
getFile = do
  loc <- qLocation
  let name = loc_module loc
  let filename = "content/" <> map toLower name
  content <- embedFile filename

  [|Text.decodeUtf8 $(pure content)|]

zipIndex :: V.Vector t -> V.Vector (Int, t)
zipIndex v = V.zip (V.enumFromN 0 (V.length v)) v

-- * Parsing

type Parser t = Parsec Void Text t

sc :: Parser ()
sc = L.space (void (char ' ')) lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

symbol_ :: Text -> Parser ()
symbol_ s = void (symbol s)

select :: [t] -> [(t, [t])]
select [] = []
select (x : xs) = (x, xs) : ((x :) <$$> select xs)

unsafeParse :: Parser t -> Text -> t
unsafeParse p s = case parse (p <* eof) "" s of
  Right res -> res
  Left e -> error (errorBundlePretty e)

-- Text utils
unsafeRead :: (Read t) => Text -> t
unsafeRead = Unsafe.fromJust . readMay . Text.unpack

unsafeRead2D :: (Read t) => Text -> [[t]]
unsafeRead2D = parse2D unsafeRead

unsafeRead1D :: (Read t) => Text -> [t]
unsafeRead1D = map unsafeRead . Text.words

class SplitLine b where
  parse2D :: (b -> a) -> Text -> [[a]]

instance SplitLine Text where
  parse2D f s = map (map f . Text.words) (Text.lines s)

instance SplitLine Char where
  parse2D f s = map (map f . Text.unpack) (Text.lines s)

parse2DGrid :: (SplitLine b) => (b -> a) -> Text -> Map (V2 Int) a
parse2DGrid f t = Map.fromList $ do
  (y, l) <- zip [0 ..] (parse2D f t)
  (x, v) <- zip [0 ..] l

  pure (V2 x y, v)

getBounds :: [V2 Int] -> (V2 Int, V2 Int)
getBounds g = do
  let minX = minimum $ map (view _x) g
  let minY = minimum $ map (view _y) g
  let maxX = maximum $ map (view _x) g
  let maxY = maximum $ map (view _y) g

  (V2 minX minY, V2 maxX maxY)

display2DGrid :: Map (V2 Int) Text -> IO ()
display2DGrid = putStrLn . Text.unpack . str2DGrid

str2DGrid :: Map (V2 Int) Text -> Text
str2DGrid g =
  let (V2 minX minY, V2 maxX maxY) = getBounds (Map.keys g)
   in Text.intercalate "\n" $ flip map [minY .. maxY] $ \y -> Text.stripEnd $ Text.intercalate "" $ flip map [minX .. maxX] $ \x -> do
    case Map.lookup (V2 x y) g of
      Nothing -> " "
      Just v -> v

flipImage :: Text -> Text
flipImage = Text.unlines . reverse . Text.lines

-- * Tests Utile

pow10 :: Int -> Int
pow10 a = 10 ^ a

parseNumber :: (Num t) => Parser t
parseNumber = fromIntegral <$> (L.signed sc (lexeme L.decimal) :: Parser Integer)

bisect :: Integral b => (b -> Bool) -> (b, b) -> (b, b)
bisect p = uncurry go
  where
    go a b
      | a + 1 == b = (a, b)
      | p mid = go mid b
      | otherwise = go a mid
      where
        mid = (a + b) `div` 2

-- | Apply f until it become stable
fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f x =
  let x' = f x
   in if x == x'
        then x'
        else fixpoint f x'

-- | @fastMatrixPower n m mat@ = (mat ^ n) `mod` m
-- Using a fast power heuristic (so n can be hyper large)
fastMatrixPower :: (Integral a1, Integral a2) => a2 -> a1 -> V2 (V2 a1) -> V2 (V2 a1)
fastMatrixPower 0 _ _ = V2 (V2 1 0) (V2 0 1)
fastMatrixPower 1 m mat = (`mod` m) <$$> mat
fastMatrixPower n m v = do
  let approximateSqrt = truncate @Double $ sqrt $ fromIntegral n
      rest = n - approximateSqrt * approximateSqrt

      sqrtMatrix = fastMatrixPower approximateSqrt m $ fastMatrixPower approximateSqrt m v
  sqrtMatrix !*! fastMatrixPower rest m v

-- reste chinois
getEs :: [Integer] -> [Integer]
getEs l = do
  (n, product -> n') <- select l

  let fact = inverseMod n' n
  pure $ fact * n'

resteChinois :: [(Integer, Integer)] -> Integer
resteChinois l =
  let es = getEs (map snd l)
   in sum $ zipWith (*) (map fst l) es

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ !x = x
applyN n f !x = applyN (n - 1) f (f x)

sum1ToN :: (Integral a) => a -> a
sum1ToN n = sum [1 .. n]

connect8 :: (Integral i) => [V2 i]
connect8 = do
  dx <- [0, 1, -1]
  V2 dx <$> [0, 1, -1]

connect4 :: (Integral i) => [V2 i]
connect4 = [V2 0 0, V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1)]

tshow :: (Show a) => a -> Text
tshow v = Text.pack (show v)

unsafeSplitOn2 :: (HasCallStack) => Text -> Text -> (Text, Text)
unsafeSplitOn2 needle t = case Text.splitOn needle t of
  [a, b] -> (a, b)
  _ -> error "splitOn2 did not returned 2 items"

-- For memoization
instance (Eq k, Hashable k) => MapLike (HashMap.HashMap k v) k v where
  lookup = HashMap.lookup
  add = HashMap.insert
