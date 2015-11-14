module MaybeHero.Inventory
( MoveableItem
, mkItem)
where

import qualified MaybeHero.GameObject as GO

data MoveableItem = MoveableItem String String [String]

instance GO.GameObject MoveableItem where
  objectName (MoveableItem name _ _) = name
  objectDescription (MoveableItem _ desc _) = desc
  objectSynonyms (MoveableItem _ _ syns) = syns

mkItem :: String -> [String] -> String -> MoveableItem
mkItem n s d = MoveableItem n d s
