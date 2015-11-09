module MaybeHero.Inventory
( MoveableItem
, mkItem)
where

import qualified MaybeHero.GameObject as GO

data MoveableItem = MoveableItem String String [String]

instance GO.GameObject MoveableItem where
  objectName (MoveableItem s _ _) = s
  objectDescription o = "a description"
  objectSynonyms o = []

mkItem s = MoveableItem s "" []
