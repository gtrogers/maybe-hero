module MaybeHero.Help (help) where

import MaybeHero.World
import MaybeHero.Command

help :: Command

help inputTokens oldWorld =
  updateLine oldWorld $
    unwords $
      map (++ "\n")
        [" Move [direction] | Move in [direction]"
        ,"Look [something] | Take a closer look at [something]"]