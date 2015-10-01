module MaybeHero.Look (look) where

import MaybeHero.Command
import MaybeHero.Player
import MaybeHero.Room

look :: Command

look inputTokens oldPlayer@(Player room _) =
  updateLine oldPlayer $ describeRoom room

