module Qecs.Entity where

import Data.IORef
import Data.Vector.Unboxed.Mutable qualified as VU
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Optics.Core
import Qecs.Component (ComponentId)

newtype Entity = Entity Word32 deriving (Eq, Ord, Show)

data Entities bits = Entities
  { components :: VU.IOVector bits,
    nextEntity :: IORef Word32
  }
  deriving (Generic)

createEntities :: Word32 -> IO (Entities Word64)
createEntities size = Entities <$> VU.replicate 0 (fromIntegral size) <*> newIORef 0

nextEntity :: Entities bits -> IO Entity
nextEntity entities = atomicModifyIORef' (entities ^. #nextEntity) (\a -> (succ a, Entity a))

modifyEntityComponents :: (VU.Unbox bits) => Entities bits -> (bits -> bits) -> Entity -> IO ()
modifyEntityComponents entities f (Entity e) = VU.modify (entities ^. #components) f (fromIntegral e)

getEntityComponents :: (VU.Unbox bits) => Entities bits -> Entity -> IO bits
getEntityComponents entities (Entity e) = VU.unsafeRead (entities ^. #components) (fromIntegral e)

class EntityComponents bits where
  hasComponent :: ComponentId -> bits -> Bool
  getComponentIds :: bits -> [ComponentId]
  setComponent :: ComponentId -> bits -> bits
  unsetComponent :: ComponentId -> bits -> bits
  clearComponents :: bits