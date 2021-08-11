module Mpv.MpvError where

import qualified Mpv.Data.MpvError as MpvError
import Mpv.Data.MpvError (MpvError (MpvError))
import Mpv.Data.Property (Property, propertyName)

amendError ::
  Text ->
  MpvError ->
  MpvError
amendError info = \case
  MpvError err ->
    MpvError (amend err)
  MpvError.Fatal err ->
    MpvError.Fatal (amend err)
  where
    amend err =
      [exon|#{info}: #{err}|]

propError ::
  Property v ->
  MpvError ->
  MpvError
propError prop =
  amendError [exon|getting #{show prop} ('#{propertyName prop}')|]

setPropError ::
  Property v ->
  MpvError ->
  MpvError
setPropError prop =
  amendError [exon|setting #{show prop} ('#{propertyName prop}')|]

optionError ::
  Text ->
  Text ->
  MpvError ->
  MpvError
optionError key value =
  amendError [exon|#{key} -> #{value}|]
