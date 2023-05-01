{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module Qecs.Component where

import Data.Proxy
import Type.Reflection (SomeTypeRep (SomeTypeRep), TypeRep, Typeable, typeRep)
import Unsafe.Coerce (unsafeCoerce)

newtype Component a = Component (TypeRep a) deriving (Show, Eq, Ord)

describeComponent :: (Typeable a) => Component a
describeComponent = Component typeRep

data SomeComponent = forall a.
  SomeComponent
  { component :: Component a
  }

instance Eq SomeComponent where
  (SomeComponent (Component component)) == (SomeComponent (Component component')) = SomeTypeRep component == SomeTypeRep component'

instance Ord SomeComponent where
  (SomeComponent (Component component)) `compare` (SomeComponent (Component component')) = SomeTypeRep component `compare` SomeTypeRep component'

fromSomeStore :: forall a. (Typeable a) => SomeComponent -> Maybe (Component a)
fromSomeStore (SomeComponent component) =
  let wantedComponent = describeComponent @a
   in if wantedComponent == unsafeCoerce component then Just (unsafeCoerce component) else Nothing