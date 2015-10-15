module MaybeHero.Command (Command) where

import qualified MaybeHero.World as World

type Command = [String] -> World.World -> World.World
