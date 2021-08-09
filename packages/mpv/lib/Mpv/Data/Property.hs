module Mpv.Data.Property where

import Mpv.Data.SubFps (SubFps)

data Property :: Type -> Type where
  Custom :: Text -> Property Value
  Duration :: Property Double
  SubFps :: Property SubFps

deriving instance Eq (Property v)
deriving instance Show (Property v)
