{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Qecs.Simulation where

import Control.Category
import Data.Functor.Identity
import Language.Haskell.TH
import Qecs.Bundle
import Qecs.Entity
import Qecs.Store.Store
import Prelude hiding ((.))

data Simulation a b where
  SimulationId :: Simulation a a
  SimulationDup :: Simulation a (a, a)
  SimulationSequence :: Simulation a b -> Simulation b c -> Simulation a c
  SimulationParallel :: Simulation a b -> Simulation c d -> Simulation (a, c) (b, d)
  SimulationPure :: Code Q a -> Simulation x a
  SimulationArr :: Code Q (a -> b) -> Simulation a b
  SimulationIO :: Code Q (a -> IO b) -> Simulation a b
  SimulationQuery :: Query Identity a b -> Simulation a b
  SimulationQueryIO :: Query IO a b -> Simulation a b
  SimulationCreateEntity :: Bundle c BundleWrite -> Simulation [c] [Entity]

data Query f a b where
  QueryMap :: Code Q (i -> f o) -> Bundle i BundleRead -> Bundle o BundleWrite -> Query f a ()
  QueryMapWithInput :: Code Q (a -> i -> f o) -> Bundle i BundleRead -> Bundle o BundleWrite -> Query f a ()
  QueryFold :: Code Q b -> Code Q (b -> i -> f b) -> Bundle i BundleRead -> Query f a b
  QueryFoldWrite :: Code Q b -> Code Q (b -> i -> f (o, b)) -> Bundle i BundleRead -> Bundle o BundleWrite -> Query f a b
  QueryFoldWithInput :: Code Q b -> Code Q (b -> a -> i -> f b) -> Bundle i BundleRead -> Query f a b
  QueryFoldWithInputAndWrite :: Code Q b -> Code Q (b -> a -> i -> f (o, b)) -> Bundle i BundleRead -> Bundle o BundleWrite -> Query f a b

cmap ::
  (GetBundle BundleRead i, GetBundle BundleWrite o) =>
  Code Q (i -> o) ->
  Simulation a ()
cmap f = SimulationQuery $ QueryMap [||Identity . $$f||] getBundle getBundle

cmapM ::
  (GetBundle BundleRead i, GetBundle BundleWrite o) =>
  Code Q (i -> IO o) ->
  Simulation a ()
cmapM f = SimulationQueryIO $ QueryMap f getBundle getBundle

cfold :: (GetBundle BundleRead i, GetBundle BundleWrite i, Monoid b) => Code Q (i -> b) -> Simulation x b
cfold f = SimulationQuery $ QueryFold [||mempty||] [||\b i -> Identity $ b <> $$f i||] getBundle

cfoldM :: (GetBundle BundleRead i, GetBundle BundleWrite i, Monoid b) => Code Q (i -> IO b) -> Simulation x b
cfoldM f = SimulationQueryIO $ QueryFold [||mempty||] [||\b i -> (b <>) <$> $$f i||] getBundle

spure :: Code Q c -> Simulation x c
spure = SimulationPure

makeEntities :: forall w c. (GetBundle BundleWrite c) => Simulation [c] [Entity]
makeEntities = SimulationCreateEntity getBundle

instance Category (Simulation) where
  id = SimulationId
  (.) = flip SimulationSequence

(***) :: Simulation a b -> Simulation c d -> Simulation (a, c) (b, d)
(***) = SimulationParallel

(&&&) :: Simulation a b -> Simulation a d -> Simulation a (b, d)
s1 &&& s2 = SimulationParallel s1 s2 . SimulationDup

(>>>) :: Simulation a b -> Simulation b c -> Simulation a c
s1 >>> s2 = SimulationSequence s1 s2

arr :: Code Q (a -> b) -> Simulation a b
arr = SimulationArr