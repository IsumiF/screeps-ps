module Main
  ( main
  , loop
  ) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as FO

main :: Effect Unit
main = pure unit

loop :: Effect Unit
loop = log "Hello"
