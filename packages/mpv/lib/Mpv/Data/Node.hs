module Mpv.Data.Node where

data Node =
  Invalid !Text
  |
  None
  |
  String !Text
  |
  Flag !Bool
  |
  Int64 !Int64
  |
  Double !Double
  |
  Array ![Node]
  |
  Map !(Map Text Node)
  |
  ByteArray !ByteString
  deriving stock (Eq, Show)
