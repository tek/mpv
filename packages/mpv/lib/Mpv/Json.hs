module Mpv.Json where

import qualified Data.Aeson as Aeson
import Data.Aeson (SumEncoding (UntaggedValue), Value, camelTo2, fromJSON)
import Data.Aeson.TH (deriveJSON)
import Data.List (dropWhileEnd)
import qualified Language.Haskell.TH as TH

basicOptions :: Aeson.Options
basicOptions =
  Aeson.defaultOptions {
    Aeson.fieldLabelModifier = dropWhile ('_' ==) . dropWhileEnd ('_' ==)
  }

jsonOptions :: Aeson.Options
jsonOptions =
  basicOptions {
    Aeson.unwrapUnaryRecords = True
  }

untaggedOptions :: Aeson.Options
untaggedOptions =
  jsonOptions {
    Aeson.sumEncoding = UntaggedValue
  }

lowerMinusJson :: TH.Name -> TH.Q [TH.Dec]
lowerMinusJson =
  deriveJSON jsonOptions {
    Aeson.unwrapUnaryRecords = True,
    Aeson.constructorTagModifier = camelTo2 '-'
  }

aesonToEither :: Aeson.Result a -> Either Text a
aesonToEither = \case
  Aeson.Success a -> Right a
  Aeson.Error s -> Left (toText s)
{-# inline aesonToEither #-}

jsonDecodeValue ::
  FromJSON a =>
  Value ->
  Either Text a
jsonDecodeValue =
  first toText . aesonToEither . fromJSON
{-# inline jsonDecodeValue #-}
