{-# options_haddock prune #-}

-- |Description: Mpv Effect
module Mpv.Effect.Mpv where

import Polysemy.Time (Seconds (Seconds))

import Mpv.Data.Command (Command, CycleDirection)
import Mpv.Data.Property (Property)

data Mpv :: Effect where
  CommandSync :: TimeUnit u => u -> Command a -> Mpv m a
  Prop :: Property v -> Mpv m v
  SetProp :: Show v => Property v -> v -> Mpv m ()
  AddProp :: Show v => Property v -> Maybe v -> Mpv m ()
  MultiplyProp :: Show v => Property v -> v -> Mpv m ()
  CycleProp :: Show v => Property v -> Maybe CycleDirection -> Mpv m ()
  SetOption :: Text -> Text ->  Mpv m ()

makeSem ''Mpv

command ::
  Member Mpv r =>
  Command a ->
  Sem r a
command =
  commandSync (Seconds 1)
