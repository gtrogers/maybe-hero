module MaybeHero.Room
( Room (..)
, describeRoom
) where

import qualified Data.Map as Map

data Room = Room String String Orientation

describeRoom :: Room -> String
describeRoom (Room name description orientation) =
  "[" ++ name ++ "]\n" 
  ++ description
  ++ (roomTransitions orientation)

type Orientation = Map.Map String Room

roomTransitions :: Orientation -> String
roomTransitions o = Map.foldWithKey addDesc "" o
  where addDesc k (Room name _ _) str = str ++ "\n  " ++ k ++ "\t| " ++ name

