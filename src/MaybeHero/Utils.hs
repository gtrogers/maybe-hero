module MaybeHero.Utils (
  reverseAndExpandTuple
, lowerCaseWords
, maybeToEither
) where

import qualified Data.Map as Map
import qualified Data.Char as C
import qualified Data.Maybe as Maybe
import qualified Data.Either as Either

addSynonym :: Ord b => a -> Map.Map b a -> b -> Map.Map b a
addSynonym cmd m syn = Map.insert syn cmd m

addSynonyms :: Ord b => Map.Map b a -> (a, [b]) -> Map.Map b a
addSynonyms m (cmd, syns) = foldl (addSynonym cmd) m syns

reverseAndExpandTuple :: Ord b => [(a, [b])] -> Map.Map b a
reverseAndExpandTuple mappings = foldl addSynonyms Map.empty mappings

toLower :: String -> String
toLower str = map C.toLower str

lowerCaseWords :: [String] -> [String]
lowerCaseWords wrds =  map toLower wrds

maybeToEither :: a -> Maybe.Maybe b -> Either a b
maybeToEither left maybeRight =
  Maybe.maybe (Left left) Right maybeRight
