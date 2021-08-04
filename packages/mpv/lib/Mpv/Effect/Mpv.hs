module Mpv.Effect.Mpv where

import Polysemy.Time (Seconds (Seconds), TimeUnit)

-- import Mpv.Data.MpvEvent (MpvEventMeta)
-- import Mpv.Data.Property (Property)
-- import Mpv.Data.PropertyFormat (PropertyRepr)

data Mpv (command :: Type -> Type) :: Effect where
  CommandSync :: TimeUnit u => u -> command a -> Mpv command m a
  CommandAsync :: command a -> Mpv command m ()
  -- Prop :: Text -> Mpv m (Either MpvError (Maybe v))
  -- SetProp :: Text -> v -> Mpv m (Either MpvError ())
  -- SetOption :: Text -> Text ->  Mpv command m ()
  -- Event :: Double -> Mpv m MpvEventMeta

makeSem ''Mpv

command ::
  Member (Mpv command) r =>
  command a ->
  Sem r a
command =
  commandSync (Seconds 1)
