module Utils.Value where

data Value =
  I Integer | Tt | Ff

instance Show Value where
  show (I i) = show i
  show Tt = show True
  show Ff = show False
