
-- file: ch13/buildmap.hs
import qualified Data.Map as Map

-- Functions to generate a Map that represents an association list
-- as a map

al = [(1, "one"), (2, "two"), (3, "three"), (4, "four")]

{- | Create a map representation of 'al' by converting the association
-  list using Map.fromList -}
mapFromAL =
    Map.fromList al

{- | Create a map representation of 'al' by doing a fold -}
mapFold =
    foldl (\map (k, v) -> Map.insert k v map) Map.empty al

{- | Manually create a map with the elements of 'al' in it -}
mapManual =
    Map.insert 2 "two" . 
    Map.insert 4 "four" .
    Map.insert 1 "one" .
    Map.insert 3 "three" $ Map.empty
