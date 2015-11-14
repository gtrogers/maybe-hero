module MaybeHero.GameObject where

import qualified Data.Map as Map
import qualified MaybeHero.Utils as Utils
import qualified Data.Maybe as Maybe

class GameObject a where
  objectName :: a -> String
  objectDescription :: a -> String
  objectSynonyms :: a -> [String]

objectToSynTuple :: GameObject a => a -> (a, [String])
objectToSynTuple o = (o, Utils.lowerCaseWords $ (objectName o):(objectSynonyms o))

objectListToSynTuples :: GameObject a => [a] -> [(a, [String])]
objectListToSynTuples = map objectToSynTuple

objectListToObjectMap :: GameObject a => [a] -> Map.Map String a
objectListToObjectMap = Utils.reverseAndExpandTuple . objectListToSynTuples

findObject :: GameObject a => String -> [a] -> Maybe a
findObject possibleSyn = Map.lookup possibleSyn . objectListToObjectMap

findDescription :: GameObject a => String -> [a] -> Maybe String
findDescription word  =  (fmap objectDescription) . (findObject word)
