module Qecs.Entity where

import Data.IORef
import Data.Word (Word32)
import GHC.Generics (Generic)

newtype Entity = Entity Word32 deriving (Eq, Ord, Show)

data EntityStore = EntityStore
  { counter :: IORef Word32
  }
  deriving (Generic)

createEntityStore :: IO EntityStore
createEntityStore = EntityStore <$> newIORef 0

nextEntity :: EntityStore -> IO Entity
nextEntity (EntityStore ref) = atomicModifyIORef' ref (\a -> (succ a, Entity a))