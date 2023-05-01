{-# LANGUAGE DerivingVia #-}

module BasicSimulation where

import Data.Monoid
import GHC.Generics
import Qecs.Simulation
import Qecs.Store.Map (MapStore, mapStoreCapabilities)

data World = World
  { position :: MapStore Position,
    velocity :: MapStore Velocity
  }
  deriving (Generic)

data Position = Position Int Int deriving (Show)

data Velocity = Velocity Int Int deriving (Show)

addSimulation :: Simulation String Int Int
addSimulation = arr [||(+ 10)||] >>> arr [||(+ 20)||]

querySimulation :: Simulation World Int Int
querySimulation =
  useStore [||\(World p _) -> p||] (mapStoreCapabilities @Position) $
    useStore [||\(World _ v) -> v||] (mapStoreCapabilities @Velocity) $
      spure [||[(Position 20 20, Velocity 0 0), (Position 20 20, Velocity 10 10)]||]
        >>> makeEntities
        >>> cmap [||\(Position a b, Velocity da db) -> Position (a + da) (b + db)||]
        >>> cfold [||\(Position a b) ->  Sum a + Sum b||]
        >>> SimulationArr [||getSum||]