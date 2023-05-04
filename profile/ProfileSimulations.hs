{-# LANGUAGE DerivingVia #-}

module ProfileSimulations where

import Data.Foldable (Foldable (..))
import Data.Monoid
import Debug.Trace
import Foreign (Storable (..), castPtr, plusPtr)
import GHC.Generics
import Qecs.Simulation
import Qecs.Store.Map (MapStore, mapStoreCapabilities)
import Qecs.Store.SparseSet

data World = World
  { position :: !(SparseSetStorable ProfilePosition),
    velocity :: !(SparseSetStorable ProfileVelocity)
  }
  deriving (Generic)

data ProfilePosition = ProfilePosition !Int !Int deriving (Show)

data ProfileVelocity = ProfileVelocity !Int !Int deriving (Show)

mapStoreSimulation :: Simulation World () Int
mapStoreSimulation =
  useStore [||\(World p _) -> p||] (sparseSetStorableCapabilities @ProfilePosition) $
    useStore [||\(World _ v) -> v||] (sparseSetStorableCapabilities @ProfileVelocity) $
      spure [||replicate 8000 (ProfileVelocity 1 1, ProfilePosition 0 0)||]
        >>> makeEntities
        >>> spure [||replicate 2000 (ProfilePosition 0 0)||]
        >>> makeEntities
        >>> SimulationPure [||()||]
        >>> foldl' (\b _ -> b >>> applyVelocity) applyVelocity [1 .. 80]
        >>> cfold [||(\(ProfilePosition a b) -> Sum a + Sum b)||]
        >>> SimulationArr [||getSum||]

applyVelocity :: Simulation w a ()
applyVelocity = cmap [||\(ProfileVelocity da db, ProfilePosition a b) -> ProfilePosition (a + da) (b + db)||]

instance Storable ProfilePosition where
  sizeOf _ = 2 * sizeOf (undefined :: Int)
  alignment _ = alignment (undefined :: Int)
  peek ptr = do
    let p = castPtr ptr
    v1 <- peek p
    v2 <- peek (p `plusPtr` sizeOf (undefined :: Int))
    pure $ ProfilePosition v1 v2
  poke ptr (ProfilePosition v1 v2) = do
    let p = castPtr ptr
    poke p v1
    poke (p `plusPtr` sizeOf (undefined :: Int)) v2

instance Storable ProfileVelocity where
  sizeOf _ = 2 * sizeOf (undefined :: Int)
  alignment _ = alignment (undefined :: Int)
  peek ptr = do
    let p = castPtr ptr
    v1 <- peek p
    v2 <- peek (p `plusPtr` sizeOf (undefined :: Int))
    pure $ ProfileVelocity v1 v2
  poke ptr (ProfileVelocity v1 v2) = do
    let p = castPtr ptr
    poke p v1
    poke (p `plusPtr` sizeOf (undefined :: Int)) v2