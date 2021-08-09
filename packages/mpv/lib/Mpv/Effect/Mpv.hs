module Mpv.Effect.Mpv where

import Polysemy.Time (Seconds (Seconds), TimeUnit)

import Mpv.Data.Property (Property)

data Mpv (command :: Type -> Type) :: Effect where
  CommandSync :: TimeUnit u => u -> command a -> Mpv command m a
  CommandAsync :: command a -> Mpv command m ()
  Prop :: Property v -> Mpv command m v
  -- SetProp :: Text -> v -> Mpv command m ()
  -- SetOption :: Text -> Text ->  Mpv command m ()

makeSem ''Mpv

command ::
  Member (Mpv command) r =>
  command a ->
  Sem r a
command =
  commandSync (Seconds 1)
