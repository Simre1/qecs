{-# LANGUAGE AllowAmbiguousTypes #-}

module Qecs.Store.Store where

import GHC.Generics (Generic)
import Qecs.Component (Component, SomeComponent (..), describeComponent)
import Qecs.Entity (Entity)
import Language.Haskell.TH
import Type.Reflection (TypeRep, Typeable, typeRep)

data StoreCapabilities s a = StoreCapabilities
  { component :: Component a,
    store :: TypeRep s,
    read :: Code Q (s -> Entity -> IO a),
    write :: Code Q (s -> Entity -> a -> IO ()),
    delete :: Code Q (s -> Entity -> IO ()),
    has :: Code Q (s -> Entity -> IO Bool),
    members :: Code Q (s -> IO Int),
    iterate :: StoreIterate s a
  }
  deriving (Generic)

data SomeStoreCapabilities where
  SomeStoreCapabilities :: StoreCapabilities s a -> SomeStoreCapabilities

data StoreIterate s a = StoreIterate (forall b. Code Q (s -> b -> (b -> Entity -> a -> IO b) -> IO b))

defaultStoreCapabilities :: forall s c. (Typeable c, Typeable s) => StoreCapabilities s c
defaultStoreCapabilities =
  StoreCapabilities
    { component = describeComponent,
      store = typeRep,
      read = unimplementedCapability @s @c "read",
      write = unimplementedCapability @s @c "write",
      delete = unimplementedCapability @s @c "delete",
      has = unimplementedCapability @s @c "has",
      members = unimplementedCapability @s @c "members",
      iterate = StoreIterate $ unimplementedCapability @s @c "iterate"
    }

unitStoreCapabilities :: Typeable s => StoreCapabilities s ()
unitStoreCapabilities =
  defaultStoreCapabilities
    { read = [||\_ _ -> pure ()||],
      write = [||\_ _ _ -> pure ()||],
      delete = [||\_ _ -> pure ()||]
    }

unimplementedCapability :: forall s c a. (Typeable s, Typeable c) => String -> Code Q a
unimplementedCapability capability =
  error $
    "Capability "
      <> capability
      <> " is not implemented for component "
      <> show (typeRep @c)
      <> " in store "
      <> show (typeRep @s)
      <> "."

-- newtype CreateSomeStore i = CreateSomeStore (i -> IO SomeStore)
