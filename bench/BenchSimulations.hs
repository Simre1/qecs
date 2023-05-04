{-# LANGUAGE DerivingVia #-}

module BenchSimulations where

import Control.DeepSeq
import Data.Foldable (Foldable (..))
import Data.Monoid
import Foreign (Storable (..), castPtr, plusPtr)
import GHC.Generics
import Qecs.Simulation
import Qecs.Store.Map (MapStore, mapStoreCapabilities)
import Qecs.Store.SparseSet
import Debug.Trace

data MapStores = MapStores
  { position :: !(MapStore BenchPosition),
    velocity :: !(MapStore BenchVelocity)
  }
  deriving (Generic)

data SparseSets = SparseSets
  { position :: !(SparseSet BenchPosition),
    velocity :: !(SparseSet BenchVelocity)
  }
  deriving (Generic)

data SparseSetsStorable = SparseSetsStorable
  { position :: !(SparseSetStorable BenchPosition),
    velocity :: !(SparseSetStorable BenchVelocity)
  }
  deriving (Generic)

instance NFData MapStores

instance NFData SparseSets

instance NFData SparseSetsStorable

data BenchPosition = BenchPosition !Int !Int deriving (Show)

instance Storable BenchPosition where
  sizeOf _ = 2 * sizeOf (undefined :: Int)
  alignment _ = alignment (undefined :: Int)
  peek ptr = do
    let p = castPtr ptr
    v1 <- peek p
    v2 <- peek (p `plusPtr` sizeOf (undefined :: Int))
    pure $ BenchPosition v1 v2
  poke ptr (BenchPosition v1 v2) = do
    let p = castPtr ptr
    poke p v1
    poke (p `plusPtr` sizeOf (undefined :: Int)) v2

  {-# INLINE sizeOf #-}
  {-# INLINE alignment #-}
  {-# INLINE poke #-}
  {-# INLINE peek #-}

data BenchVelocity = BenchVelocity !Int !Int deriving (Show)

instance Storable BenchVelocity where
  sizeOf _ = 2 * sizeOf (undefined :: Int)
  alignment _ = alignment (undefined :: Int)
  peek ptr = do
    let p = castPtr ptr
    v1 <- peek p
    v2 <- peek (p `plusPtr` sizeOf (undefined :: Int))
    pure $ BenchVelocity v1 v2
  poke ptr (BenchVelocity v1 v2) = do
    let p = castPtr ptr
    poke p v1
    poke (p `plusPtr` sizeOf (undefined :: Int)) v2
    
  {-# INLINE sizeOf #-}
  {-# INLINE alignment #-}
  {-# INLINE poke #-}
  {-# INLINE peek #-}
  
allocateEntitiesMapStoreBench :: Simulation MapStores () ()
allocateEntitiesMapStoreBench =
  useStore [||\(MapStores p _) -> p||] (mapStoreCapabilities @BenchPosition) $
    useStore [||\(MapStores _ v) -> v||] (mapStoreCapabilities @BenchVelocity) $
      spure [||replicate 1000 (BenchPosition 0 0, BenchVelocity 1 1)||]
        >>> makeEntities
        >>> spure [||replicate 9000 (BenchPosition 0 0)||]
        >>> makeEntities
        >>> SimulationPure [||()||]

cmapMapStoreBench :: Simulation MapStores () Int
cmapMapStoreBench =
  useStore [||\(MapStores p _) -> p||] (mapStoreCapabilities @BenchPosition) $
    useStore [||\(MapStores _ v) -> v||] (mapStoreCapabilities @BenchVelocity) $
      foldl' (\b _ -> b >>> applyVelocity) applyVelocity [1 .. 5]
        >>> cfold [||(\(BenchPosition a b) -> Sum a + Sum b)||]
        >>> SimulationArr [||getSum||]

allocateEntitiesSparseSetBench :: Simulation SparseSets () ()
allocateEntitiesSparseSetBench =
  useStore [||\(SparseSets p _) -> p||] (sparseSetCapabilities @BenchPosition) $
    useStore [||\(SparseSets _ v) -> v||] (sparseSetCapabilities @BenchVelocity) $
      spure [||replicate 1000 (BenchPosition 0 0, BenchVelocity 1 1)||]
        >>> makeEntities
        >>> spure [||replicate 9000 (BenchPosition 0 0)||]
        >>> makeEntities
        >>> SimulationPure [||()||]

cmapSparseSetBench :: Simulation SparseSets () Int
cmapSparseSetBench =
  useStore [||\(SparseSets p _) -> p||] (sparseSetCapabilities @BenchPosition) $
    useStore [||\(SparseSets _ v) -> v||] (sparseSetCapabilities @BenchVelocity) $
      foldl' (\b _ -> b >>> applyVelocity) applyVelocity [1 .. 5]
        >>> cfold [||(\(BenchPosition a b) -> Sum a + Sum b)||]
        >>> SimulationArr [||getSum||]

allocateEntitiesSparseSetStorableBench :: Simulation SparseSetsStorable () ()
allocateEntitiesSparseSetStorableBench =
  useStore [||\(SparseSetsStorable p _) -> p||] (sparseSetStorableCapabilities @BenchPosition) $
    useStore [||\(SparseSetsStorable _ v) -> v||] (sparseSetStorableCapabilities @BenchVelocity) $
      spure [||replicate 1000 (BenchPosition 0 0, BenchVelocity 1 1)||]
        >>> makeEntities
        >>> spure [||replicate 9000 (BenchPosition 0 0)||]
        >>> makeEntities
        >>> SimulationPure [||()||]

cmapSparseSetStorableBench :: Simulation SparseSetsStorable () Int
cmapSparseSetStorableBench =
  useStore [||\(SparseSetsStorable p _) -> p||] (sparseSetStorableCapabilities @BenchPosition) $
    useStore [||\(SparseSetsStorable _ v) -> v||] (sparseSetStorableCapabilities @BenchVelocity) $
      foldl' (\b _ -> b >>> applyVelocity) applyVelocity [1 .. 5]
        >>> cfold [||(\(BenchPosition a b) -> Sum a + Sum b)||]
        >>> SimulationArr [||getSum||]

applyVelocity :: Simulation w a ()
applyVelocity = cmap [||\(BenchPosition a b, BenchVelocity da db) -> BenchPosition (a + da) (b + db)||]