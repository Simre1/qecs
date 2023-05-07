{-# LANGUAGE AllowAmbiguousTypes #-}

module Qecs.Compile.Compile where

import Control.Applicative ((<|>))
import Control.HigherKindedData
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Data (Data (gmapQl), Proxy (Proxy), Typeable)
import Data.Kind (Type)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Data.Typeable (cast)
import Data.Vector qualified as V
import Debug.Trace
import Language.Haskell.TH (Code (..), Q)
import Language.Haskell.TH qualified as TH
import LiftType (typeRepToType)
import Optics.Core
import Qecs.Bundle (Bundle (..), BundleRead (..), BundleWrite (..))
import Qecs.Compile.CreateQuery (WriteF (..), compileQuery, compileQueryIO, writeBundle)
import Qecs.Compile.Environment
import Qecs.Compile.Optimize (optimize)
import Qecs.Component
import Qecs.Entity
import Qecs.Simulation
import Qecs.Store.Store
  ( RuntimeStore,
    SomeStoreCapabilities (SomeStoreCapabilities),
    Store,
    StoreCapabilities (component),
  )
import Type.Reflection (SomeTypeRep (SomeTypeRep), TypeRep (..), someTypeRep, typeRep, withTypeable)
import Unsafe.Coerce (unsafeCoerce)

viewCode :: Code Q a -> Code Q String
viewCode code = Code $ do
  str <- TH.pprint . TH.unType <$> examineCode code
  examineCode [||str||]

newtype Coded m a = Coded (Code Q (m a))

compile :: (HTraversable w, MonadIO m) => w (Store m) -> Simulation a b -> Code Q (m WorldEnvironment, WorldEnvironment -> a -> IO b)
compile world simulation =
  let compileEnvironment = generateCompileEnvironment world
      (simulate, _) =
        runCompileM compileEnvironment CompileState $
          generateSimulate simulation
   in [||
      ( $$(generateWorldEnvironment world),
        \worldEnvironment -> runExecutionM worldEnvironment . $$(simulate)
      )
      ||]
  where

    generateSimulate :: forall a b. Simulation a b -> CompileM (Code Q (a -> ExecutionM b))
    generateSimulate = \case
      SimulationArr f -> pure [||pure . $$(f)||]
      SimulationPure x -> pure [||pure . const $$x||]
      SimulationIO f -> pure [||liftIO . $$(f)||]
      SimulationSequence s1 s2 -> do
        gS1 <- generateSimulate s1
        gS2 <- generateSimulate s2
        pure [||$$gS1 >=> $$gS2||]
      SimulationParallel s1 s2 -> do
        gS1 <- generateSimulate s1
        gS2 <- generateSimulate s2
        pure [||\(a, b) -> (,) <$> $$(gS1) a <*> $$(gS2) b||]
      SimulationQuery q -> compileQuery q
      SimulationQueryIO qIO -> compileQueryIO qIO
      SimulationCreateEntity bundle -> do
        (WriteF write) <- writeBundle bundle
        pure
          [||
          \bundles -> do
            write' <- $$write
            entityStore <- getEntities
            for bundles $ \bundle -> liftIO $ do
              entity <- nextEntity entityStore
              write' entity bundle
              pure entity
          ||]
      _ -> undefined