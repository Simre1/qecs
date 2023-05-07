{-# LANGUAGE DerivingVia #-}

module ProfileSimulations where

import Control.HigherKindedData
import Data.Foldable (Foldable (..))
import Data.Monoid
import Debug.Trace
import Foreign (Storable (..), castPtr, plusPtr)
import GHC.Generics
import Qecs.Simulation
import Qecs.Store.Map (MapStore, mapStoreCapabilities)
import Qecs.Store.SparseSet

data World f = World
  { position :: f Position,
    velocity :: f Velocity
  }
  deriving (Generic)

instance HTraversable World where
  htraverse f (World p v) = World <$> f p <*> f v

data Position = Position !Int !Int deriving (Show)

instance Storable Position where
  sizeOf _ = 2 * sizeOf (undefined :: Int)
  alignment _ = alignment (undefined :: Int)
  peek ptr = do
    let p = castPtr ptr
    v1 <- peek p
    v2 <- peek (p `plusPtr` sizeOf (undefined :: Int))
    pure $ Position v1 v2
  poke ptr (Position v1 v2) = do
    let p = castPtr ptr
    poke p v1
    poke (p `plusPtr` sizeOf (undefined :: Int)) v2

  {-# INLINE sizeOf #-}
  {-# INLINE alignment #-}
  {-# INLINE poke #-}
  {-# INLINE peek #-}

data Velocity = Velocity !Int !Int deriving (Show)

instance Storable Velocity where
  sizeOf _ = 2 * sizeOf (undefined :: Int)
  alignment _ = alignment (undefined :: Int)
  peek ptr = do
    let p = castPtr ptr
    v1 <- peek p
    v2 <- peek (p `plusPtr` sizeOf (undefined :: Int))
    pure $ Velocity v1 v2
  poke ptr (Velocity v1 v2) = do
    let p = castPtr ptr
    poke p v1
    poke (p `plusPtr` sizeOf (undefined :: Int)) v2

  {-# INLINE sizeOf #-}
  {-# INLINE alignment #-}
  {-# INLINE poke #-}
  {-# INLINE peek #-}

mapStoreSimulation :: Simulation () Int
mapStoreSimulation =
  spure [||replicate 8000 (Velocity 1 1, Position 0 0)||]
    >>> makeEntities
    >>> spure [||replicate 2000 (Position 0 0)||]
    >>> makeEntities
    >>> SimulationPure [||()||]
    >>> foldl' (\b _ -> b >>> applyVelocity) applyVelocity [1 .. 80]
    >>> cfold [||(\(Position a b) -> Sum a + Sum b)||]
    >>> SimulationArr [||getSum||]

applyVelocity :: Simulation a ()
applyVelocity = cmap [||\(Velocity da db, Position a b) -> Position (a + da) (b + db)||]
