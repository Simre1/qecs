{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Qecs.Resource where

import Data.Bundle
import Data.Functor.Identity
import Language.Haskell.TH
import Optics.Core
import Qecs.Compile.Environment
import Qecs.Component (Component, describeComponent)
import Qecs.Entity
import Qecs.ExecutionM
import Qecs.Query (BundleDelete, BundleRead, BundleWrite)
import Qecs.Store.Store
import Type.Reflection (Typeable)

newtype Get a = Get (Entity -> IO a)

newtype Put a = Put (Entity -> a -> IO ())

newtype Delete a = Delete (Entity -> IO ())

newtype New a = New (a -> IO Entity)

newtype DeleteEntity = DeleteEntity (Entity -> IO ())

data Resource a where
  ResourceGet :: Bundle a BundleRead -> Resource (Get a)
  ResourceWrite :: Bundle a BundleWrite -> Resource (Put a)
  ResourceDelete :: Bundle a BundleDelete -> Resource (Delete a)
  ResourceNew :: Bundle a BundleWrite -> Resource (New a)
  ResourceDeleteEntity :: Resource DeleteEntity

instance (GetBundle BundleRead a) => GetBundle' True Resource (Get a) where
  getBundle' = BundleSingle $ ResourceGet getBundle

instance (GetBundle BundleWrite a) => GetBundle' True Resource (New a) where
  getBundle' = BundleSingle $ ResourceNew getBundle

instance (GetBundle BundleWrite a) => GetBundle' True Resource (Put a) where
  getBundle' = BundleSingle $ ResourceWrite getBundle

instance (GetBundle BundleDelete a) => GetBundle' True Resource (Delete a) where
  getBundle' = BundleSingle (ResourceDelete getBundle)

instance GetBundle' True Resource DeleteEntity where
  getBundle' = BundleSingle ResourceDeleteEntity
