module MaybeHero.Inventory
( Item
, mkItem)
where

data Item = Item String

instance Show Item where
  show (Item s) = s

mkItem s = Item s
