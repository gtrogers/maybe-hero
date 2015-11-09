module MaybeHero.Inventory
( MoveableItem
, mkItem)
where

data MoveableItem = MoveableItem String

instance Show MoveableItem where
  show (MoveableItem s) = s

mkItem s = MoveableItem s
