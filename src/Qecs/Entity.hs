module Qecs.Entity where

import Data.IORef
import Data.Vector.Primitive qualified as VP
import Data.Vector.Primitive.Mutable qualified as VP
import Data.Word (Word32)
import GHC.Generics (Generic)
import Optics.Core

newtype Entity = Entity Word32 deriving (Eq, Ord, Show)

data Entities = Entities
  { entityList :: VP.IOVector Word32,
    nextEntity :: IORef Word32,
    lastEntity :: IORef Word32
  }
  deriving (Generic)

createEntities :: Word32 -> IO Entities
createEntities size = do
  entityList <- VP.thaw $ VP.fromList ([1 .. fromIntegral size - 1] ++ [0])
  nextEntity <- newIORef 0
  lastEntity <- newIORef (fromIntegral size)
  pure $ Entities entityList nextEntity lastEntity

createEntity :: Entities -> IO Entity
createEntity entities = do
  next <- readIORef (entities ^. #nextEntity)
  nextNext <- VP.read (entities ^. #entityList) (fromIntegral next)
  writeIORef (entities ^. #nextEntity) nextNext
  pure $ Entity next

removeEntity :: Entities -> Entity -> IO ()
removeEntity entities (Entity entity) = do
  next <- readIORef (entities ^. #nextEntity)
  VP.write (entities ^. #entityList) (fromIntegral entity) next
  writeIORef (entities ^. #nextEntity) entity
