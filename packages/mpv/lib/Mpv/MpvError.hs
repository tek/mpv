module Mpv.MpvError where

import qualified Mpv.Data.MpvError as MpvError
import Mpv.Data.MpvError (MpvError (MpvError))
import Mpv.Data.Property (Property, propertyName)

propError ::
  Property v ->
  MpvError ->
  MpvError
propError prop = \case
  MpvError err ->
    MpvError (amend err)
  MpvError.Fatal err ->
    MpvError.Fatal (amend err)
  where
    amend err =
      [exon|setting #{show prop} ('#{propertyName prop}'): #{err}|]

optionError ::
  Text ->
  Text ->
  MpvError ->
  MpvError
optionError key value = \case
  MpvError err ->
    MpvError [exon|#{key} -> #{value}: #{err}|]
  e ->
    e
