module Main
  ( loop
  ) where

import Prelude
import Effect (Effect)
import Effect.Console (log)

loop :: Effect Unit
loop = log "Looping"
