module MaybeHero.WorldLoader (parseWorldFromFile)
where

import qualified MaybeHero.World as W
import qualified MaybeHero.Room as R
import qualified Data.Yaml.YamlLight as Y
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as C8
import qualified Data.Maybe as Maybe
import Control.Monad ((>>), (>>=), sequence)

type ParseError = String

yamlKey :: String -> Y.YamlLight
yamlKey = Y.YStr . C8.pack

getMapVal :: String -> Y.YamlLight -> Maybe Y.YamlLight
getMapVal s y = do
  m <- Y.unMap y
  M.lookup (yamlKey s) m

getMapStrVal :: String -> Y.YamlLight -> Either ParseError String
getMapStrVal s y =
  Maybe.maybe (Left "Failure to parse string") (Right . C8.unpack) maybeElement
  where maybeElement = (getMapVal s y) >>= Y.unStr

parseRoom :: Y.YamlLight -> Either ParseError R.Room
parseRoom y = do
  name <- (getMapStrVal "name" y)
  description <- (getMapStrVal "description" y)
  Right $ R.mkRoom name description M.empty []

parseRooms :: Y.YamlLight -> Either ParseError [R.Room]
parseRooms y = case Y.unSeq y of
  Just seq -> sequence $ map parseRoom seq
  Nothing -> Left "Cannot read seq"

parseWorld :: Y.YamlLight -> W.World
parseWorld yml = case (parseRooms yml) of
  Left parseError -> error parseError
  Right rooms -> W.World (R.roomName (head rooms))

-- Thoughts -> store rooms in a map of id to function that returns a room when given orientations
  -- i.e. need some way of post-linking the constructed rooms

parseWorldFromFile :: String -> IO W.World
parseWorldFromFile s = do
  f <- readFile s
  y <- Y.parseYaml f
  return (parseWorld y)
