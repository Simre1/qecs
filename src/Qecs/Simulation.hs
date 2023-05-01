{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Qecs.Simulation where

import Control.Category
import Qecs.Bundle
import Qecs.Entity
import Qecs.Store.Store
import Language.Haskell.TH
import Optics.Core
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
  SimulationQuery :: Code Q (b -> x -> b) -> Code Q b -> Code Q (a -> i -> (o, x)) -> Bundle BundleRead i -> Bundle BundleWrite o -> Simulation w a b
  SimulationIOQuery :: Code Q (b -> x -> b) -> Code Q b -> Code Q (a -> i -> IO (o, x)) -> Bundle BundleRead i -> Bundle BundleWrite o -> Simulation w a b
  SimulationUseStore :: Code Q (w -> s) -> StoreCapabilities s c -> Simulation w a b -> Simulation w a b
  SimulationCreateEntity :: Bundle BundleWrite c -> Simulation w [c] [Entity]

query ::
  forall a i o b w.
  (GetBundle BundleRead i, GetBundle BundleWrite o) =>
  Code Q b ->
  Code Q (b -> b -> b) ->
  Code Q (a -> i -> (o, b)) ->
  Simulation w a b
query acc accumulate f =
  SimulationQuery
    accumulate
    acc
    f
    (getBundle @(BaseCase i) @BundleRead @i)
    (getBundle @(BaseCase o) @BundleWrite @o)

queryIO ::
  forall a i o b w.
  (GetBundle BundleRead i, GetBundle BundleWrite o) =>
  Code Q b ->
  Code Q (b -> b -> b) ->
  Code Q (a -> i -> IO (o, b)) ->
  Simulation w a b
queryIO acc accumulate f =
  SimulationIOQuery
    accumulate
    acc
    f
    (getBundle @(BaseCase i) @BundleRead @i)
    (getBundle @(BaseCase o) @BundleWrite @o)

cmap ::
  (GetBundle BundleRead i, GetBundle BundleWrite o) =>
  Code Q (i -> o) ->
  Simulation w a ()
cmap f = query [||()||] [||\_ _ -> ()||] [||\_ i -> ($$f i, ())||]

cmapM ::
  (GetBundle BundleRead i, GetBundle BundleWrite o) =>
  Code Q (i -> IO o) ->
  Simulation w a ()
cmapM f = queryIO [||()||] [||\_ _ -> ()||] [||\_ i -> (,()) <$> $$f i||]

cfold :: (GetBundle BundleRead i, GetBundle BundleWrite i, Monoid b) => Code Q (i -> b) -> Simulation w x b
cfold f = query [||mempty||] [||(<>)||] [||\_ i -> (i, $$f i)||]

cfoldM :: (GetBundle BundleRead i, GetBundle BundleWrite i, Monoid b) => Code Q (i -> IO b) -> Simulation w x b
cfoldM f = queryIO [||mempty||] [||(<>)||] [||\_ i -> (i,) <$> $$f i||]

spure :: Code Q c -> Simulation w x c
spure = SimulationPure

useStore :: Code Q (w -> s) -> StoreCapabilities s c -> Simulation w a b -> Simulation w a b
useStore = SimulationUseStore

makeEntities :: forall w c. (GetBundle BundleWrite c) => Simulation w [c] [Entity]
makeEntities = SimulationCreateEntity (getBundle @(BaseCase c) @BundleWrite @c)

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