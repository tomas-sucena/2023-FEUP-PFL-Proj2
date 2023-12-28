module Utils.Value where

data Value =
  I Integer | B Bool

instance Show Value where
  show (I i) = show i
  show (B b) = show b
