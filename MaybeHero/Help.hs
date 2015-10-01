module MaybeHero.Help (help) where

import MaybeHero.Player
import MaybeHero.Command

help :: Command

help inputTokens oldPlayer =
  updateLine oldPlayer $
    unwords $ 
      map (++ "\n")
        [" Move [direction] | Move in [direction]"
        ,"Look             | Print room information"]
