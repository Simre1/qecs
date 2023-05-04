{-# LANGUAGE DerivingVia #-}

module Qecs.Store.Map where

import Control.DeepSeq (NFData)
import Data.Data
import Data.IORef
import Data.IntMap.Strict qualified as IM
import Qecs.Entity
import Qecs.Store.Store (StoreCapabilities (..), defaultStoreCapabilities)
import Unsafe.Coerce

newtype MapStore a = MapStore (IORef (IM.IntMap a)) deriving (Typeable, NFData)

mapStore :: IO (MapStore a)
mapStore = MapStore <$> newIORef IM.empty

toInt :: Entity -> Int
toInt (Entity entity) = fromIntegral entity

mapStoreCapabilities :: (Typeable a) => StoreCapabilities (MapStore a) a
mapStoreCapabilities =
  defaultStoreCapabilities
    { read = [||\(MapStore ref) entity -> (IM.! (toInt entity)) <$> readIORef ref||],
      write = [||\(MapStore ref) entity value -> modifyIORef' ref $ IM.insert (toInt entity) value||],
      delete = [||\(MapStore ref) entity -> modifyIORef' ref $ IM.delete (toInt entity)||],
      has = [||\(MapStore ref) entity -> IM.member (toInt entity) <$> readIORef ref||],
      members = [||\(MapStore ref) -> IM.size <$> readIORef ref||],
      iterate =
        [||
        \(MapStore ref) f ->
          readIORef ref
            >>= IM.foldlWithKey' (\previous entity a -> previous >> f (Entity $ fromIntegral entity) a) (pure ())
        ||]
    }