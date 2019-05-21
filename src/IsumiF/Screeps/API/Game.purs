module IsumiF.Screeps.API.Game
  ( module IsumiF.Screeps.API.Types
  , game
  , CallStructureSpawn(..)
  , class IsStructure
  , spawnCreep
  , class IsStructureSpawn
  , destroy
  ) where

import Prelude
import Effect (Effect)

import IsumiF.Screeps.API.Types

foreign import game :: Game

class IsStructure a where
  destroy :: a -> Effect Unit

class IsStructure a <= IsStructureSpawn a where
  spawnCreep :: a -> Effect Unit

newtype CallStructureSpawn = CallStructureSpawn StructureSpawn

instance spawnIsStructure :: IsStructure CallStructureSpawn where
  destroy (CallStructureSpawn x) = structureDestroy x

instance spawnIsStructureSpawn :: IsStructureSpawn CallStructureSpawn where 
  spawnCreep _ = pure unit

foreign import structureDestroy :: forall sub. Structure sub -> Effect Unit

newtype CallCreep = CallCreep Creep

class IsCreep a where
  moveToTarget :: a -> RoomPosition -> Effect Unit

class HasRoomPosition a where
  getRoomPosition :: a -> RoomPosition

instance roomPosition :: HasRoomPosition RoomPosition where
  getRoomPosition = identity

instance roomObjectHasRoomPosition :: HasRoomPosition (RoomObject sub) where
  getRoomPosition roomObject = roomObject.pos
