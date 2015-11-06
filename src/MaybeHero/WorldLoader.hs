module MaybeHero.WorldLoader (
  parseWorldFromFile
) where

import qualified MaybeHero.Scenery as S
import qualified MaybeHero.Room as R
import qualified MaybeHero.World as W
import qualified MaybeHero.Inventory as I
import qualified Data.Either as Either
import MaybeHero.Yaml ((££), (£), (£!), Parseable, ParseError, parseYaml, parseFromFile)
import MaybeHero.Utils (maybeToEither)

instance Parseable I.Item where
  parseYaml y = do
    name <- parseYaml y
    return $ I.mkItem name

instance Parseable S.Scenery where
  parseYaml y =
    S.mkScenery
    ££ ("name", y)
    £  ("synonyms", y)
    £  ("description", y)

instance Parseable R.Room where
  parseYaml y =
    R.mkRoom
    ££ ("name", y)
    £  ("description", y)
    £  ("orientation", y)
    £! ("scenery", [], y)
    £! ("items", [], y)

instance Parseable W.World where
  parseYaml y =
    W.mkWorld
    ££ ("rooms", y)
    £  ("currentRoom", y)

validateWorld :: W.World -> Either ParseError W.World
validateWorld w = do
    (const w) <$> validationResult
    where validateRoom = (\o -> maybeToEither ("No room with name " ++ o) (W.lookupRoomStrict o w))
          validationResult = sequence $ map validateRoom $ W.roomDestinations w

parseWorldFromFile :: String -> IO W.World
parseWorldFromFile s = do
  w <- parseFromFile s
  return $ Either.either error id $ w >>= validateWorld -- throw error if parsing fails
