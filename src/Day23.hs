module Day23 where

import Utils
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sortBy, find, maximumBy)
import Data.Ord (comparing)

fileContent = parseContent $(getFile)

parseContent = map (\x -> let (a, b) = (Text.breakOn "-" x)
                          in (a, Text.drop 1 b)) . Text.lines
 

-- * Generics
mkLinkMap links = Map.fromListWith (<>) $ do
          (a, b) <- links
          [(a, Set.singleton b), (b, Set.singleton a)]

-- * FIRST problem
day links = length $ Set.fromList $ do
  let linkMap = mkLinkMap links

  -- Pick a link starting with `t`
  (firstLink, secondLinks) <- Map.toList linkMap
  guard $ Text.head firstLink == 't'

  secondLink <- Set.toList secondLinks

  -- To be sure there is no self link
  guard $ firstLink /= secondLink

  -- Pick the third item
  case Map.lookup secondLink linkMap of
    Nothing -> []
    Just thirdLinks -> do
       thirdLink <- Set.toList thirdLinks
       guard $ thirdLink /= firstLink && thirdLink /= secondLink
       guard $ thirdLink `Set.member` secondLinks
       pure $ Set.fromList [firstLink, secondLink, thirdLink]

-- * SECOND problem
findMax links = maximumBy (comparing length) $ do
  let linkMap = mkLinkMap links
  (node, links) <- Map.toList linkMap
  
  -- Let's find all the combination we may care about
  -- They are sorted by reverse order of length, so it lou
  let validateAll oneSet = all (\copain -> do
         -- oneSet contains all the nodes in the tentative set, except "node"
         -- Which is find, because we KNOW that all nodes we are trying ARE pointing to node
         let copaincopain = linkMap Map.! copain

         -- I know that this POINT to the original item, that's part of the
         -- list, let's remove it
         let copaincopain' = Set.delete node copaincopain
         if oneSet `Set.isSubsetOf` (Set.insert copain copaincopain')
         then True
         else False) (Set.toList oneSet)
             
  let pset = reverse $ sortBy (comparing length) $ Set.toList $ Set.powerSet links
  let res = find validateAll pset
  case res of
    Nothing -> []
    Just v -> pure (Set.insert node v)

day' links = Text.intercalate "," $ sort $ Set.toList $ findMax links

ex = parseContent [str|\
kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn
|]

ex' = parseContent [str|\
ka-co
ta-co
de-co
ta-ka
de-ta
ka-de
|]
-- started at Tue Dec 24 08:04:40 AM +04 2024
