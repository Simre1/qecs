{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Qecs.Bundle where

import Control.HigherKindedData
import Data.Functor.Identity
import Language.Haskell.TH
import Qecs.Component (Component, describeComponent)
import Type.Reflection (Typeable)

newtype BundleRead a = BundleRead (Component a) deriving (Eq, Show)

newtype BundleWrite a = BundleWrite (Component a) deriving (Eq, Show)

data Bundle a f where
  BundleEmpty :: Bundle () f
  BundleSingle :: f a -> Bundle a f
  BundleTwo :: Bundle a f -> Bundle b f -> Bundle (a, b) f
  BundleThree :: Bundle a f -> Bundle b f -> Bundle c f -> Bundle (a, b, c) f

instance HFunctor (Bundle a) where
  hmap f = runIdentity . htraverse (Identity . f)

instance HTraversable (Bundle a) where
  htraverse _ BundleEmpty = pure BundleEmpty
  htraverse f (BundleSingle a) = BundleSingle <$> f a
  htraverse f (BundleTwo a b) = BundleTwo <$> htraverse f a <*> htraverse f b
  htraverse f (BundleThree a b c) =
    BundleThree
      <$> htraverse f a
      <*> htraverse f b
      <*> htraverse f c

instance HFoldable Identity Bundle where
  hfold _ empty BundleEmpty = empty
  hfold _ _ (BundleSingle f) = f
  hfold combine empty (BundleTwo a b) =
    combine
      (VariableFunction (Identity id) (Identity id))
      (hfold combine empty a)
      (hfold combine empty b)
  hfold combine empty (BundleThree a b c) =
    combine
      (VariableFunction (Identity (\((a, b), c) -> (a, b, c))) (Identity (\(a, b, c) -> ((a, b), c))))
      ( combine
          (VariableFunction (Identity id) (Identity id))
          (hfold combine empty a)
          (hfold combine empty b)
      )
      (hfold combine empty c)

instance HFoldable (Code Q) Bundle where
  hfold _ empty BundleEmpty = empty
  hfold _ _ (BundleSingle f) = f
  hfold combine empty (BundleTwo a b) =
    combine
      (VariableFunction [||id||] [||id||])
      (hfold combine empty a)
      (hfold combine empty b)
  hfold combine empty (BundleThree a b c) =
    combine
      (VariableFunction [||(\((a, b), c) -> (a, b, c))||] [||(\(a, b, c) -> ((a, b), c))||])
      ( combine
          (VariableFunction [||id||] [||id||])
          (hfold combine empty a)
          (hfold combine empty b)
      )
      (hfold combine empty c)

type family BaseCase a where
  BaseCase () = False
  BaseCase (a, b) = False
  BaseCase (a, b, c) = False
  BaseCase (a, b, c, d) = False
  BaseCase (a, b, c, d, e) = False
  BaseCase _ = True

type GetBundle f a = GetBundle' (BaseCase a) f a

getBundle :: forall f a. (GetBundle f a) => Bundle a f
getBundle = getBundle' @(BaseCase a) @f @a

class GetBundle' (baseCase :: Bool) f i where
  getBundle' :: Bundle i f

instance (GetBundle f a, GetBundle f b) => GetBundle' False f (a, b) where
  getBundle' = BundleTwo (getBundle' @(BaseCase a) @f @a) (getBundle' @(BaseCase b) @f @b)

instance (GetBundle f a, GetBundle f b, GetBundle f c) => GetBundle' False f (a, b, c) where
  getBundle' =
    BundleThree
      (getBundle' @(BaseCase a) @f @a)
      (getBundle' @(BaseCase b) @f @b)
      (getBundle' @(BaseCase c) @f @c)

instance (Typeable a) => GetBundle' True BundleRead a where
  getBundle' = BundleSingle $ BundleRead $ describeComponent @a

instance (Typeable a) => GetBundle' True BundleWrite a where
  getBundle' = BundleSingle $ BundleWrite $ describeComponent @a

instance GetBundle' False f () where
  getBundle' = BundleEmpty
