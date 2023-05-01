{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Qecs.Bundle where

import Data.Functor.Identity
import Qecs.Component (Component, describeComponent)
import Language.Haskell.TH
import Type.Reflection (Typeable)
import GHC.Generics (Generic)

newtype BundleRead a = BundleRead (Component a) deriving (Eq, Show)

newtype BundleWrite a = BundleWrite (Component a) deriving (Eq, Show)

data Bundle f a where
  BundleEmpty :: Bundle f ()
  BundleSingle :: f a -> Bundle f a
  BundleTwo :: Bundle f a -> Bundle f b -> Bundle f (a, b)
  BundleThree :: Bundle f a -> Bundle f b -> Bundle f c -> Bundle f (a, b, c)

bundleMapF :: (forall x. f x -> g x) -> Bundle f a -> Bundle g a
bundleMapF f = runIdentity . bundleTraverseF (Identity . f)

bundleTraverseF :: (Applicative m) => (forall x. f x -> m (g x)) -> Bundle f a -> m (Bundle g a)
bundleTraverseF _ BundleEmpty = pure BundleEmpty
bundleTraverseF f (BundleSingle a) = BundleSingle <$> f a
bundleTraverseF f (BundleTwo a b) = BundleTwo <$> bundleTraverseF f a <*> bundleTraverseF f b
bundleTraverseF f (BundleThree a b c) =
  BundleThree
    <$> bundleTraverseF f a
    <*> bundleTraverseF f b
    <*> bundleTraverseF f c

data VariableFunction a b = VariableFunction
  { coFunction :: Code Q (a -> b),
    contraFunction :: Code Q (b -> a)
  } deriving Generic

bundleFoldF :: (forall x y z. VariableFunction (x, y) z -> f x -> f y -> f z) -> f () -> Bundle f a -> f a
bundleFoldF _ empty BundleEmpty = empty
bundleFoldF _ _ (BundleSingle f) = f
bundleFoldF combine empty (BundleTwo a b) =
  combine
    (VariableFunction [||id||] [||id||])
    (bundleFoldF combine empty a)
    (bundleFoldF combine empty b)
bundleFoldF combine empty (BundleThree a b c) =
  combine
    (VariableFunction [||(\((a, b), c) -> (a, b, c))||] [||(\(a, b, c) -> ((a, b), c))||])
    ( combine
        (VariableFunction [||id||] [||id||])
        (bundleFoldF combine empty a)
        (bundleFoldF combine empty b)
    )
    (bundleFoldF combine empty c)


type family BaseCase a where
  BaseCase () = False
  BaseCase (a, b) = False
  BaseCase (a, b, c) = False
  BaseCase (a, b, c, d) = False
  BaseCase (a, b, c, d, e) = False
  BaseCase _ = True

type GetBundle f a = GetBundle' (BaseCase a) f a

class GetBundle' (baseCase :: Bool) f i where
  getBundle :: Bundle f i

instance (GetBundle f a, GetBundle f b) => GetBundle' False f (a, b) where
  getBundle = BundleTwo (getBundle @(BaseCase a) @f @a) (getBundle @(BaseCase b) @f @b)

instance (GetBundle f a, GetBundle f b, GetBundle f c) => GetBundle' False f (a, b, c) where
  getBundle =
    BundleThree
      (getBundle @(BaseCase a) @f @a)
      (getBundle @(BaseCase b) @f @b)
      (getBundle @(BaseCase c) @f @c)

instance (Typeable a) => GetBundle' True BundleRead a where
  getBundle = BundleSingle $ BundleRead $ describeComponent @a

instance (Typeable a) => GetBundle' True BundleWrite a where
  getBundle = BundleSingle $ BundleWrite $ describeComponent @a

instance GetBundle' False f () where
  getBundle = BundleEmpty
