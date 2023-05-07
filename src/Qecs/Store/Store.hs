{-# LANGUAGE AllowAmbiguousTypes #-}

module Qecs.Store.Store where

import Control.HigherKindedData
import Data.Constraint (Constraint, Dict, withDict)
import Data.Set qualified as S
import GHC.Generics (Generic)
import Language.Haskell.TH
import Optics.Core
import Qecs.Component (Component, SomeComponent (..), describeComponent)
import Qecs.Entity (Entity)
import Type.Reflection (TypeRep, Typeable, typeRep)
import Unsafe.Coerce (unsafeCoerce)

data StoreCapabilities s a = StoreCapabilities
  { component :: Component a,
    store :: TypeRep s,
    read :: Code Q (s a -> Entity -> IO a),
    write :: Code Q (s a -> Entity -> a -> IO ()),
    delete :: Code Q (s a -> Entity -> IO ()),
    has :: Code Q (s a -> Entity -> IO Bool),
    members :: Code Q (s a -> IO Int),
    iterate :: Code Q (s a -> (Entity -> a -> IO ()) -> IO ())
  }
  deriving (Generic)

data Store f a where
  Store :: StoreCapabilities s a -> Code Q (f (s a)) -> Store f a

data SomeStore f = forall a. SomeStore (Store f a)

data RuntimeStore

makeRuntimeStore :: (Functor f) => Store f a -> Code Q (f RuntimeStore)
makeRuntimeStore (Store _ create) =
  [||unsafeCoerce <$> $$create||]

componentForStore :: Store f a -> Component a
componentForStore (Store sc _) = sc ^. #component

-- data ArchetypeCapabilities s a = MatchCapabilities {
--   members :: Archetype -> Code Q (s -> IO ArchetypeSize),
--   read :: Archetype -> Code Q (s -> ArchetypeEntity -> IO a),
--   readWithEntity :: Archetype -> Code Q (s -> ArchetypeEntity -> IO ()),
--   insert :: Archetype -> Code Q (s -> Entity -> a -> IO ())
-- }

-- newtype ArchetypeSize = ArchetypeSize Word32 deriving (Eq, Ord)

-- newtype ArchetypeEntity = ArchetypeEntity Word32

data Archetype = Archetype
  { id :: Int,
    types :: S.Set SomeComponent
  }
  deriving (Generic, Eq, Ord)

data SomeStoreCapabilities where
  SomeStoreCapabilities :: StoreCapabilities s a -> SomeStoreCapabilities

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
      iterate = unimplementedCapability @s @c "iterate"
    }

unitStoreCapabilities :: (Typeable s) => StoreCapabilities s ()
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
