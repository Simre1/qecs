{-# LANGUAGE DerivingVia #-}

module TestSimulations where

import Control.HigherKindedData (HTraversable (htraverse))
import Data.Monoid
import GHC.Generics
import Qecs.Simulation
import Qecs.Store.Map
import Qecs.Store.Store

data World f = World
  { position :: f Position,
    velocity :: f Velocity
  }
  deriving (Generic)

world :: World (Store IO)
world =
  World
    { position = mapStore,
      velocity = mapStore
    }

instance HTraversable World where
  htraverse f (World p v) = World <$> f p <*> f v

data Position = Position Int Int deriving (Show)

data Velocity = Velocity Int Int deriving (Show)

addSimulation :: Simulation Int Int
addSimulation = arr [||(+ 10)||] >>> arr [||(+ 20)||]

querySimulation :: Simulation Int Int
querySimulation =
  spure [||[(Position 20 20, Velocity 0 0), (Position 20 20, Velocity 10 10)]||]
    >>> makeEntities
    >>> cmap [||\(Position a b, Velocity da db) -> Position (a + da) (b + db)||]
    >>> cfold [||\(Position a b) -> Sum a + Sum b||]
    >>> SimulationArr [||getSum||]