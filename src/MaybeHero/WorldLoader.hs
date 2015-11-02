module MaybeHero.WorldLoader (parseWorldFromFile)
where

import qualified MaybeHero.World as W
import qualified MaybeHero.Room as R
import qualified MaybeHero.Scenery as S
import qualified Data.Yaml.YamlLight as Y
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as C8
import qualified Data.Maybe as Maybe
import Control.Monad ((>>), (>>=), sequence)

type ParseError = String
type Yaml = Y.YamlLight
type YamlMap = M.Map Y.YamlLight Y.YamlLight
type YamlParser a = (Yaml -> Either ParseError a)

yamlKey :: String -> Y.YamlLight
yamlKey = Y.YStr . C8.pack

parseString :: YamlParser String
parseString y = Maybe.maybe (Left "Failure to parse string") (Right . C8.unpack) $ Y.unStr y

parseSeq :: YamlParser a -> YamlParser [a]
parseSeq p y = Maybe.maybe (Left "Failure to parse sequence") (sequence .(map p)) $ Y.unSeq y

parseMap :: YamlParser YamlMap
parseMap y = Maybe.maybe (Left "Failure to parse map") Right $ Y.unMap y

parseMapValue :: String -> YamlParser a -> YamlMap -> Either ParseError a
parseMapValue s p m = Maybe.maybe (Left "FAILED") p $ M.lookup (yamlKey s) m

parseMapValueOptional :: String -> YamlParser a -> a -> YamlMap -> Either ParseError a
parseMapValueOptional s p d m = Maybe.maybe (Right d) p $ M.lookup (yamlKey s) m

parseMapEntry :: (Ord a) => (Ord b) => YamlParser a -> YamlParser b -> (Yaml, Yaml) -> Either ParseError (a, b)
parseMapEntry kp vp (ky, vy) = do
  k <- kp ky
  v <- vp vy
  return (k, v)

parseMapKeysAndValues :: (Ord a) => (Ord b) => YamlParser a -> YamlParser b -> YamlParser (M.Map a b)
parseMapKeysAndValues kp vp y = do
  m <- parseMap y
  l <- sequence $ map (parseMapEntry kp vp) $ M.toList m
  return (M.fromList l)

---------DOMAIN--SPECIFIC--STUFF-----------

parseScenery :: YamlParser S.Scenery
parseScenery y = do
  m <- parseMap y
  name        <- parseMapValue "name" parseString m
  description <- parseMapValue "description" parseString m
  synonyms    <- parseMapValue "synonyms" (parseSeq parseString) m
  Right $ S.mkScenery name synonyms description

parseRoom :: YamlParser R.Room
parseRoom y = do
  m <- parseMap y
  name        <- parseMapValue "name" parseString m
  description <- parseMapValue "description" parseString m
  sceneryList <- parseMapValueOptional "scenery" (parseSeq parseScenery) [] m
  orientation <- parseMapValue "orientation" (parseMapKeysAndValues parseString parseString) m
  Right $ R.mkRoom name description orientation sceneryList

parseWorld :: Y.YamlLight -> W.World
parseWorld yml = case (parseSeq parseRoom yml) of
  Left parseError -> error parseError -- FIXME don't lob an error out at this error
  Right rooms -> W.mkWorld rooms (R.roomName (head rooms))

parseWorldFromFile :: String -> IO W.World
parseWorldFromFile s = do
  f <- readFile s
  y <- Y.parseYaml f
  return (parseWorld y)
