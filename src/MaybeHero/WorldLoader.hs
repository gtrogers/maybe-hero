module MaybeHero.WorldLoader (
  parseWorldFromFile
) where

import qualified MaybeHero.Scenery as S
import qualified MaybeHero.Room as R
import qualified MaybeHero.World as W
import MaybeHero.Yaml ((££), (£), (£!), Parseable, ParseError, parseYaml, parseFromFile)
import qualified Data.Either as Either
import MaybeHero.Utils (maybeToEither)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M

instance Parseable S.Scenery where
  parseYaml y =
    mkScenery
    ££ ("name", y)
    £  ("synonyms", y)
    £  ("description", y)

instance Parseable R.Room where
  parseYaml y =
    mkRoom
    ££ ("name", y)
    £  ("description", y)
    £  ("orientation", y)
    £! ("scenery", [], y)

instance Parseable W.World where
  parseYaml y =
    mkWorld
    ££ ("currentRoom", y)
    £  ("rooms", y)

mkScenery :: C8.ByteString -> [C8.ByteString] -> C8.ByteString -> S.Scenery
mkScenery name synonyms description =
  S.mkScenery (C8.unpack name) (map C8.unpack synonyms) (C8.unpack description)

mkRoom :: C8.ByteString -> C8.ByteString -> (M.Map C8.ByteString C8.ByteString) -> [S.Scenery] -> R.Room
mkRoom name desc orientation scenery =
  R.mkRoom (C8.unpack name) (C8.unpack desc) ((mapBoth C8.unpack C8.unpack) orientation) scenery

mkWorld :: C8.ByteString -> M.Map C8.ByteString R.Room -> W.World
mkWorld s mr = W.mkWorld (M.mapKeys C8.unpack mr) (C8.unpack s)

roomDestinations :: W.World -> [String]
roomDestinations w = do
  room <- W.rooms w
  orientation <- M.elems $ R.roomOrientation room
  return orientation

validateWorld :: W.World -> Either ParseError W.World
validateWorld w = do
    (\a -> w) <$> validationResult
    where validateRoom = (\o -> maybeToEither ("No room with name " ++ o) (W.lookupRoomStrict o w))
          validationResult = sequence $ map validateRoom $ roomDestinations w

parseWorldFromFile :: String -> IO W.World
parseWorldFromFile s = do
  w <- parseFromFile s
  return $ Either.either error id $ w >>= validateWorld -- throw error if parsing fails

mapBoth :: (Ord a) => (Ord b) => (a -> b) -> (c -> d) -> M.Map a c -> M.Map b d
mapBoth kf vf = (M.map vf) . (M.mapKeys kf)
