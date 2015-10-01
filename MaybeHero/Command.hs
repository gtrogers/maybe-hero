module MaybeHero.Command (Command) where

import MaybeHero.Player

type Command = [String] -> Player -> Player

