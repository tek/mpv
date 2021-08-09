module Mpv.Data.Property where

data Property :: Type -> Type where
  Custom :: Text -> Property Value
  Duration :: Property Double

deriving instance Eq (Property v)
deriving instance Show (Property v)
