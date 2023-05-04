{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Qecs.Simulation where

import Control.Category
import Data.Functor.Identity
import Language.Haskell.TH
import Optics.Core
import Qecs.Bundle
import Qecs.Entity
import Qecs.Store.Store
import Type.Reflection (TypeRep, Typeable, typeRep)
import Prelude hiding ((.))

data Simulation w a b where
  SimulationId :: Simulation w a a
  SimulationDup :: Simulation w a (a, a)
  SimulationSequence :: Simulation w a b -> Simulation w b c -> Simulation w a c
  SimulationParallel :: Simulation w a b -> Simulation w c d -> Simulation w (a, c) (b, d)
  SimulationPure :: Code Q a -> Simulation w x a
  SimulationArr :: Code Q (a -> b) -> Simulation w a b
  SimulationIO :: Code Q (a -> IO b) -> Simulation w a b
  SimulationQuery :: Query Identity a b -> Simulation w a b
  SimulationQueryIO :: Query IO a b -> Simulation w a b
  SimulationUseStore :: Code Q (w -> s) -> StoreCapabilities s c -> Simulation w a b -> Simulation w a b
  SimulationCreateEntity :: Bundle BundleWrite c -> Simulation w [c] [Entity]

data Query f a b where
  QueryMap :: Code Q (i -> f o) -> Bundle BundleRead i -> Bundle BundleWrite o -> Query f a ()
  QueryMapWithInput :: Code Q (a -> i -> f o) -> Bundle BundleRead i -> Bundle BundleWrite o -> Query f a ()
  QueryFold :: Code Q b -> Code Q (b -> i -> f b) -> Bundle BundleRead i -> Query f a b
  QueryFoldWrite :: Code Q b -> Code Q (b -> i -> f (o, b)) -> Bundle BundleRead i -> Bundle BundleWrite o -> Query f a b
  QueryFoldWithInput :: Code Q b -> Code Q (b -> a -> i -> f b) -> Bundle BundleRead i -> Query f a b
  QueryFoldWithInputAndWrite :: Code Q b -> Code Q (b -> a -> i -> f (o, b)) -> Bundle BundleRead i -> Bundle BundleWrite o -> Query f a b

-- query ::
--   forall a i o b w.
--   (GetBundle BundleRead i, GetBundle BundleWrite o) =>
--   Code Q b ->
--   Code Q (b -> b -> b) ->
--   Code Q (a -> i -> (o, b)) ->
--   Simulation w a b
-- query acc accumulate f =
--   SimulationQuery
--     accumulate
--     acc
--     f
--     (getBundle @(BaseCase i) @BundleRead @i)
--     (getBundle @(BaseCase o) @BundleWrite @o)

-- queryIO ::
--   forall a i o b w.
--   (GetBundle BundleRead i, GetBundle BundleWrite o) =>
--   Code Q b ->
--   Code Q (b -> b -> b) ->
--   Code Q (a -> i -> IO (o, b)) ->
--   Simulation w a b
-- queryIO acc accumulate f =
--   SimulationIOQuery
--     accumulate
--     acc
--     f
--     (getBundle @(BaseCase i) @BundleRead @i)
--     (getBundle @(BaseCase o) @BundleWrite @o)

cmap ::
  (GetBundle BundleRead i, GetBundle BundleWrite o) =>
  Code Q (i -> o) ->
  Simulation w a ()
cmap f = SimulationQuery $ QueryMap [||Identity . $$f||] getBundle getBundle

cmapM ::
  (GetBundle BundleRead i, GetBundle BundleWrite o) =>
  Code Q (i -> IO o) ->
  Simulation w a ()
cmapM f = SimulationQueryIO $ QueryMap f getBundle getBundle

cfold :: (GetBundle BundleRead i, GetBundle BundleWrite i, Monoid b) => Code Q (i -> b) -> Simulation w x b
cfold f = SimulationQuery $ QueryFold [||mempty||] [||\b i -> Identity $ b <> $$f i||] getBundle

cfoldM :: (GetBundle BundleRead i, GetBundle BundleWrite i, Monoid b) => Code Q (i -> IO b) -> Simulation w x b
cfoldM f = SimulationQueryIO $ QueryFold [||mempty||] [||\b i -> (b <>) <$> $$f i||] getBundle

spure :: Code Q c -> Simulation w x c
spure = SimulationPure

useStore :: Code Q (w -> s) -> StoreCapabilities s c -> Simulation w a b -> Simulation w a b
useStore = SimulationUseStore

makeEntities :: forall w c. (GetBundle BundleWrite c) => Simulation w [c] [Entity]
makeEntities = SimulationCreateEntity getBundle

instance Category (Simulation w) where
  id = SimulationId
  (.) = flip SimulationSequence

(***) :: Simulation w a b -> Simulation w c d -> Simulation w (a, c) (b, d)
(***) = SimulationParallel

(&&&) :: Simulation w a b -> Simulation w a d -> Simulation w a (b, d)
s1 &&& s2 = SimulationParallel s1 s2 . SimulationDup

(>>>) :: Simulation w a b -> Simulation w b c -> Simulation w a c
s1 >>> s2 = SimulationSequence s1 s2

arr :: Code Q (a -> b) -> Simulation w a b
arr = SimulationArr