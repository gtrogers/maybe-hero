module MaybeHero.Move (move) where

import qualified Data.Map as Map

import MaybeHero.Player
import MaybeHero.Room

type Command = [String] -> Player -> Player

move :: Command

move [] oldPlayer = updateLine oldPlayer "I can't move without a direction"

move inputTokens oldPlayer@(Player (Room _ _ orientation) _) =
  case (Map.lookup (unwords inputTokens) orientation) of
    Nothing -> updateLine oldPlayer "You can't go that way..."
    Just room -> moveRoom oldPlayer room

