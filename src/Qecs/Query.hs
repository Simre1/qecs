module Qecs.Query where
import Language.Haskell.TH
import Qecs.Component
import Data.Data
import Data.Bundle

newtype BundleRead a = BundleRead (Component a) deriving (Eq, Show)

newtype BundleWrite a = BundleWrite (Component a) deriving (Eq, Show)

newtype BundleDelete a = BundleDelete (Component a) deriving (Eq, Show)

instance (Typeable a) => GetBundle' True BundleRead a where
  getBundle' = BundleSingle $ BundleRead $ describeComponent @a

instance (Typeable a) => GetBundle' True BundleWrite a where
  getBundle' = BundleSingle $ BundleWrite $ describeComponent @a

data Query f a b where
  QueryMap :: Code Q (i -> f o) -> Bundle i BundleRead -> Bundle o BundleWrite -> Query f a ()
  QueryMapWithInput :: Code Q (a -> i -> f o) -> Bundle i BundleRead -> Bundle o BundleWrite -> Query f a ()
  QueryFold :: Code Q b -> Code Q (b -> i -> f b) -> Bundle i BundleRead -> Query f a b
  QueryFoldWrite :: Code Q b -> Code Q (b -> i -> f (o, b)) -> Bundle i BundleRead -> Bundle o BundleWrite -> Query f a b
  QueryFoldWithInput :: Code Q b -> Code Q (b -> a -> i -> f b) -> Bundle i BundleRead -> Query f a b
  QueryFoldWithInputAndWrite :: Code Q b -> Code Q (b -> a -> i -> f (o, b)) -> Bundle i BundleRead -> Bundle o BundleWrite -> Query f a b
