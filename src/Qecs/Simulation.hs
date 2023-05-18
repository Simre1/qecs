{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Qecs.Simulation where

import Control.Category
import Data.Bundle
import Data.Functor.Identity
import Language.Haskell.TH
import Qecs.Entity
import Qecs.ExecutionM
import Qecs.Resource
import Qecs.Query
import Qecs.Store.Store
import Prelude hiding ((.))

data Simulation a b where
  SimulationId :: Simulation a a
  SimulationDup :: Simulation a (a, a)
  SimulationSequence :: Simulation a b -> Simulation b c -> Simulation a c
  SimulationParallel :: Simulation a b -> Simulation c d -> Simulation (a, c) (b, d)
  SimulationPure :: Code Q a -> Simulation x a
  SimulationArr :: Code Q (a -> b) -> Simulation a b
  SimulationIO :: Bundle rs Resource -> Code Q (rs -> a -> IO b) -> Simulation a b
  SimulationQuery :: Query Identity a b -> Simulation a b
  SimulationQueryIO :: Query IO a b -> Simulation a b

-- SimulationAction :: Actions actions => Code Q (a -> actions -> ExecutionM b) -> Simulation a b

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

instance Category Simulation where
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

arrM :: Code Q (a -> IO b) -> Simulation a b
arrM f = SimulationIO getBundle [||\() -> $$f||]

arrMR :: (GetBundle Resource rs) => Code Q (rs -> a -> IO b) -> Simulation a b
arrMR f = SimulationIO getBundle [||$$f||]