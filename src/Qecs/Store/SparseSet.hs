module Qecs.Store.SparseSet where

import Control.DeepSeq
import Data.SparseSet.Boxed qualified as SB
import Data.SparseSet.Storable qualified as SS
import Data.SparseSet.Unboxed qualified as SU
import Foreign (Storable)
import Qecs.Entity
import Qecs.Store.Store
import Type.Reflection (Typeable)

newtype SparseSet a = SparseSet (SB.SparseSetBoxed a) deriving (NFData)

-- TODO: Need to make the sparse size according to the size of the entity store
sparseSetStore :: IO (SparseSet a)
sparseSetStore = SparseSet <$> SB.create 50000 10000

sparseSetCapabilities :: (Typeable a) => StoreCapabilities (SparseSet a) a
sparseSetCapabilities =
  defaultStoreCapabilities
    { read = [||\(SparseSet sparseSet) (Entity key) -> SB.unsafeLookup sparseSet key||],
      write = [||\(SparseSet sparseSet) (Entity key) value -> SB.insert sparseSet key value||],
      delete = [||\(SparseSet sparseSet) (Entity key) -> SB.remove sparseSet key||],
      has = [||\(SparseSet sparseSet) (Entity key) -> SB.unsafeContains sparseSet key||],
      members = [||\(SparseSet sparseSet) -> SB.size sparseSet||],
      iterate =
          [||
          \(SparseSet sparseSet) f ->
            SB.iterate sparseSet (\key val -> f (Entity key) val)
          ||]
    }

newtype SparseSetStorable a = SparseSetStorable (SS.SparseSetStorable a) deriving (NFData)

-- TODO: Need to make the sparse size according to the size of the entity store
sparseSetStorableStore :: (Storable a) => IO (SparseSetStorable a)
sparseSetStorableStore = SparseSetStorable <$> SS.create 50000 10000

sparseSetStorableCapabilities :: (Typeable a, Storable a) => StoreCapabilities (SparseSetStorable a) a
sparseSetStorableCapabilities =
  defaultStoreCapabilities
    { read = [||\(SparseSetStorable sparseSet) (Entity key) -> SS.unsafeLookup sparseSet key||],
      write = [||\(SparseSetStorable sparseSet) (Entity key) value -> SS.insert sparseSet key value||],
      delete = [||\(SparseSetStorable sparseSet) (Entity key) -> SS.remove sparseSet key||],
      has = [||\(SparseSetStorable sparseSet) (Entity key) -> SS.unsafeContains sparseSet key||],
      members = [||\(SparseSetStorable sparseSet) -> SS.size sparseSet||],
      iterate =
          [||
          \(SparseSetStorable sparseSet) f ->
            SS.iterate sparseSet (\key val -> f (Entity key) val)
          ||]
    }

newtype SparseSetUnboxed a = SparseSetUnboxed (SU.SparseSetUnboxed a) deriving (NFData)

-- TODO: Need to make the sparse size according to the size of the entity store
sparseSetUnboxedStore :: (SU.Unbox a) => IO (SparseSetUnboxed a)
sparseSetUnboxedStore = SparseSetUnboxed <$> SU.create 50000 10000

sparseSetUnboxedCapabilities :: (Typeable a, SU.Unbox a) => StoreCapabilities (SparseSetUnboxed a) a
sparseSetUnboxedCapabilities =
  defaultStoreCapabilities
    { read = [||\(SparseSetUnboxed sparseSet) (Entity key) -> SU.unsafeLookup sparseSet key||],
      write = [||\(SparseSetUnboxed sparseSet) (Entity key) value -> SU.insert sparseSet key value||],
      delete = [||\(SparseSetUnboxed sparseSet) (Entity key) -> SU.remove sparseSet key||],
      has = [||\(SparseSetUnboxed sparseSet) (Entity key) -> SU.unsafeContains sparseSet key||],
      members = [||\(SparseSetUnboxed sparseSet) -> SU.size sparseSet||],
      iterate =
          [||
          \(SparseSetUnboxed sparseSet) f ->
            SU.iterate sparseSet (\key val -> f (Entity key) val)
          ||]
    }