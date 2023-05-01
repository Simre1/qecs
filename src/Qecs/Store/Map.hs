{-# LANGUAGE DerivingVia #-}
module Qecs.Store.Map where

import Data.IORef
import Data.Map qualified as M
import Qecs.Entity
import Qecs.Store.Store (StoreCapabilities (..), StoreIterate (..), defaultStoreCapabilities)
import Data.Data

newtype MapStore a = MapStore (IORef (M.Map Entity a)) deriving Typeable

mapStore :: IO (MapStore a)
mapStore = MapStore <$> newIORef M.empty

mapStoreCapabilities :: (Typeable a) => StoreCapabilities (MapStore a) a
mapStoreCapabilities =
  defaultStoreCapabilities
    { read = [||\(MapStore ref) entity -> (M.! entity) <$> readIORef ref||],
      write = [||\(MapStore ref) entity value -> modifyIORef ref $ M.insert entity value||],
      delete = [||\(MapStore ref) entity -> modifyIORef ref $ M.delete entity||],
      has = [||\(MapStore ref) entity -> M.member entity <$> readIORef ref||],
      members = [||\(MapStore ref) -> M.size <$> readIORef ref||],
      iterate =
        StoreIterate
          [||
          \(MapStore ref) acc f ->
            readIORef ref
              >>= M.foldlWithKey' (\previous entity a -> previous >>= \b -> f b entity a) (pure acc)
          ||]
    }