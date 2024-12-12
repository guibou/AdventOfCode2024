{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import Test.Syd
import Test.Syd.OptParse (getSettings)
import qualified Data.Map.Strict as Map
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import qualified Data.Text as Text
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BS

import Discover

extractStar (Text.words -> [_, star, _]) = star

main = do
  sets <- getSettings
  res <- sydTestResult sets spec

  let 
    flattened :: Map.Map Text.Text (Map.Map Text.Text Double)
    flattened = Map.fromListWith (<>)
                  $ map (\([name, _, star_component], v) -> (name, Map.singleton (extractStar star_component) ((fromIntegral ( timedTime $ testDefVal v) / (10^(6 :: Int) :: Double)) :: Double)))
                  $ filter (\([_, step, _], _) -> step == Text.pack "works")
                  $ flattenSpecForest $ timedValue res
  
  -- Write bar plot
  toFile def "bench.svg" $ do
    layout_title .= "Advent of code 2023 - Time elapsed (ms)"
    layout_title_style . font_size .= 10
    layout_x_axis . laxis_generate .= autoIndexAxis (map (take 5) $ map Text.unpack $ Map.keys flattened)
    plot (fmap plotBars $ bars ["First star", "Second star"] (addIndexes (map (\x -> Map.elems x) $ Map.elems flattened)))

  -- Encode to json, and add the sum of all runs
  let all = sum $ map (sum . Map.elems) $ Map.elems flattened
  let flattened' = Map.insert "_all" all (fmap (sum . Map.elems) flattened)

  BS.writeFile "bench.json" (encodePretty flattened')
