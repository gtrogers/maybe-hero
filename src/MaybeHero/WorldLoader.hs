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

yamlKey :: String -> Y.YamlLight
yamlKey = Y.YStr . C8.pack

parseString :: Y.YamlLight -> Either ParseError String
parseString y = Maybe.maybe (Left "Failure to parse string") (Right . C8.unpack) $ Y.unStr y

parseSeq :: Y.YamlLight -> Either ParseError [Y.YamlLight]
parseSeq y = Maybe.maybe (Left "Failure to parse sequence") Right $ Y.unSeq y

parseMap :: Y.YamlLight -> Either ParseError (M.Map Y.YamlLight Y.YamlLight)
parseMap y = Maybe.maybe (Left "Failure to parse map") Right $ Y.unMap y

yLookup :: String -> (M.Map Y.YamlLight Y.YamlLight) -> Either ParseError Y.YamlLight
yLookup s m = Maybe.maybe (Left "Failure to lookup from map") Right $ M.lookup (yamlKey s) m

lookupString :: String -> Y.YamlLight -> Either ParseError String
lookupString s y = (parseMap y) >>= (yLookup s) >>= parseString

parseMapEntry :: (Y.YamlLight, Y.YamlLight) -> Either ParseError (String, String)
parseMapEntry (ky, vy) = do
  k <- parseString ky
  v <- parseString vy
  return (k, v)

parseStringMap :: Y.YamlLight -> Either ParseError (M.Map String String)
parseStringMap y = do
  m <- parseMap y
  l <- sequence $ map parseMapEntry $ M.toList m
  return (M.fromList l)

parseOrientation :: Y.YamlLight -> Either ParseError (M.Map String String)
parseOrientation = parseStringMap

parseScenery :: Y.YamlLight -> Either ParseError S.Scenery
parseScenery y = do
  m <- parseMap y
  name <- lookupString "name" y
  description <- lookupString "description" y
  synonymsYaml <- parseMap y >>= (yLookup "synonyms") >>= parseSeq
  synonyms <- sequence (map parseString synonymsYaml)
  Right $ S.mkScenery name synonyms description

parseSceneryList :: [Y.YamlLight] -> Either ParseError [S.Scenery]
parseSceneryList ys = sequence . (map parseScenery) $ ys

parseRoom :: Y.YamlLight -> Either ParseError R.Room
parseRoom y = do
  m <- parseMap y
  name <- lookupString "name" y
  description <- lookupString "description" y
  sceneryList <- (yLookup "scenery" m) >>= parseSeq >>= parseSceneryList
  orientation <- parseMap y >>= (yLookup "orientation") >>= parseOrientation
  Right $ R.mkRoom name description orientation sceneryList

parseRooms :: Y.YamlLight -> Either ParseError [R.Room]
parseRooms y = case Y.unSeq y of
  Just seq -> sequence $ map parseRoom seq
  Nothing -> Left "Cannot read seq"

parseWorld :: Y.YamlLight -> W.World
parseWorld yml = case (parseRooms yml) of
  Left parseError -> error parseError
  Right rooms -> W.World (R.roomName (head rooms))

parseWorldFromFile :: String -> IO W.World
parseWorldFromFile s = do
  f <- readFile s
  y <- Y.parseYaml f
  return (parseWorld y)
