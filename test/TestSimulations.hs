{-# LANGUAGE DerivingVia #-}

module TestSimulations where

import Control.HigherKindedData (HTraversable (htraverse))
import Data.Monoid
import GHC.Generics
import Qecs.Resource
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
  arrMR
    [||
    \(New new) _ ->
      traverse
        new
        [ (Position 20 20, Velocity 5 5),
          (Position 20 20, Velocity 10 10)
        ]
    ||]
    >>> cmap [||\(Position a b, Velocity da db) -> Position (a + da) (b + db)||]
    >>> cfold [||\(Position a b) -> Sum a + Sum b||]
    >>> SimulationArr [||getSum||]

resourceSimulation :: Simulation () Int
resourceSimulation =
  arrMR [||\(New new) _ -> new (Position 10 10, Velocity 1 1)||]
    >>> arrMR
      [||
      ( \(Get get) entity -> do
          (Position a b, Velocity c d) <- get entity
          pure $ sum [a, b, c, d]
      )
      ||]