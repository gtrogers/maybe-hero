module MaybeHero.Utils (
  reverseAndExpandTuple
, lowerCaseWords
, maybeToEither
, headMaybe
) where

import qualified Data.Map as Map
import qualified Data.Char as C
import qualified Data.Maybe as Maybe
import qualified Data.Either as Either

expandSynonyms :: Ord b => [(a, [b])] -> [(b, a)]
expandSynonyms xs = [(b, a) | (a, bs) <- xs, b <- bs]

reverseAndExpandTuple :: Ord b => [(a, [b])] -> Map.Map b a
reverseAndExpandTuple = Map.fromList . expandSynonyms

toLower :: String -> String
toLower str = map C.toLower str

lowerCaseWords :: [String] -> [String]
lowerCaseWords wrds =  map toLower wrds

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither left maybeRight =
  Maybe.maybe (Left left) Right maybeRight

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:xs) = Just x
