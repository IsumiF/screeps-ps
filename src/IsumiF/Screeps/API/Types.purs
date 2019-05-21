module IsumiF.Screeps.API.Types
  ( Game
  , Creep
  , RoomPosition
  , Room
  , RoomObject
  , Structure
  , StructureSpawn
  ) where

import Foreign.Object as FO

type Game =
  { creeps :: FO.Object Creep
  }

type RoomPosition =
  { x :: Number
  , y :: Number
  , roomName :: String
  }

type Room =
  { energyAvailable :: Number
  , energyCapacityAvailable :: Number
  }

type RoomObject sub =
  { -- effects :: RoomEffect
    pos :: RoomPosition
  , room :: Room
  | sub
  }

type Creep = RoomObject
  ( carryCapacity :: Number
  )

type Structure sub = RoomObject
  ( hits :: Number
  , hitsMax :: Number
  , id :: String
  , structureType :: String
  | sub
  )

type StructureSpawn = Structure
  ( energy :: Number
  , energyCapacity :: Number
  )
