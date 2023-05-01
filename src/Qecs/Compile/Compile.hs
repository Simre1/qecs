{-# LANGUAGE AllowAmbiguousTypes #-}

module Qecs.Compile.Compile where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Data (Data (gmapQl), Proxy (Proxy), Typeable)
import Data.Kind (Type)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Data.Typeable (cast)
import Data.Vector qualified as V
import Qecs.Bundle (Bundle (..), BundleRead (..), BundleWrite (..))
import Qecs.Compile.CreateQuery (WriteF (..), compileQuery, writeBundle, compileIOQuery)
import Qecs.Compile.Environment
import Qecs.Component
import Qecs.Entity
import Qecs.Simulation
import Qecs.Store.Store
import Language.Haskell.TH (Code (..), Q)
import Language.Haskell.TH qualified as TH
import LiftType (typeRepToType)
import Optics.Core
import Type.Reflection (SomeTypeRep (SomeTypeRep), TypeRep (..), someTypeRep, typeRep, withTypeable)
import Unsafe.Coerce (unsafeCoerce)
import Debug.Trace

viewCode :: Code Q a -> Code Q String
viewCode code = Code $ do
  str <- TH.pprint . TH.unType <$> examineCode code
  examineCode [||str||]

compile :: forall w a b. (Typeable w) => Simulation w a b -> Code Q (w -> IO WorldEnvironment, WorldEnvironment -> a -> IO b)
compile simulation = Code $ do
  let computeWorldEnvironment = generateComputeState compileState
      (simulate, compileState) =
        runCompileM initialCompileEnvironment initialCompileState $
          generateSimulate simulation
  examineCode
    [||
    ( $$(computeWorldEnvironment),
      \worldEnvironment -> runExecutionM worldEnvironment . $$(simulate)
    )
    ||]
  where
    initialCompileState = CompileState 0 [||M.empty||]
    initialCompileEnvironment = CompileEnvironment M.empty
    generateSimulate :: forall a b. Simulation w a b -> CompileM w (Code Q (a -> ExecutionM b))
    generateSimulate = \case
      SimulationArr f -> pure [||pure . $$(f)||]
      SimulationPure x -> pure [||pure . const $$x||]
      SimulationIO f -> pure [||liftIO . $$(f)||]
      SimulationSequence s1 s2 -> do
        gS1 <- generateSimulate s1
        gS2 <- generateSimulate s2
        pure [||$$(gS1) >=> $$(gS2)||]
      SimulationParallel s1 s2 -> do
        gS1 <- generateSimulate s1
        gS2 <- generateSimulate s2
        pure [||\(a, b) -> (,) <$> $$(gS1) a <*> $$(gS2) b||]
      SimulationUseStore
        getStore
        storeCapabilities
        simulationWithAdditionalStore -> do
          compileState <- getCompileState
          let component = storeCapabilities ^. #component
              storeIndex = compileState ^. #nextStoreIndex
          modifyCompileState $
            #nextStoreIndex %~ succ
          modifyCompileState $
            #neededStores %~ addStore getStore storeIndex

          localCompileEnvironment
            (#activeStores %~ M.insert (SomeComponent component) (storeIndex, SomeStoreCapabilities storeCapabilities))
            (generateSimulate simulationWithAdditionalStore)
      
      SimulationQuery accumulate acc queryF queryIn queryOut ->
        compileQuery accumulate acc queryF queryIn queryOut
      SimulationIOQuery accumulate acc queryF queryIn queryOut ->
        compileIOQuery accumulate acc queryF queryIn queryOut
      SimulationCreateEntity bundle -> do
        (WriteF write) <- writeBundle bundle
        pure
          [||
          \bundles -> do
            write' <- $$write
            entityStore <- getEntityStore
            for bundles $ \bundle -> liftIO $ do
              entity <- nextEntity entityStore
              write' entity bundle
              pure entity
          ||]
      _ -> undefined

    generateComputeState :: CompileState w -> Code Q (w -> IO WorldEnvironment)
    generateComputeState compileState =
      let neededStoresCode = compileState ^. #neededStores
       in [||
          \w -> do
            let size = M.size storesCode
                storesCode = $$neededStoresCode
                runtimeStores = V.generate size $ \i ->
                  (storesCode M.! i) w
            WorldEnvironment runtimeStores <$> createEntityStore
          ||]

-- getSomeTypeRep :: Some2F StoreCapabilities -> SomeTypeRep
-- getSomeTypeRep (Some2F _ storeRep _) = toSomeTypeRep storeRep

-- getStore :: Data world => world -> Some2F StoreCapabilities -> s
-- getStore world (Some2F storageCapabilities _ _) = getStore' world (unsafeCoerce $ storageCapabilities ^. #store)

-- getStore :: (Data world) => SomeTypeRep -> Code Q (world -> s)
-- getStore (SomeTypeRep tr) = getStore' tr

-- getStore' :: (Data world) => TypeRep s -> Code Q (world -> s)
-- getStore' tr = TH.unTypeCode (withTypeable tr [||extractFromWorld||])

extractFromWorld :: forall a world. (Typeable a, Data world) => world -> a
extractFromWorld world =
  case gmapQl (<|>) Nothing cast world of
    Just a -> a
    Nothing -> error $ "World " <> show (typeRep @world) <> " does not contain " <> show (typeRep @a) <> "."

addStore ::
  forall world s.
  Code Q (world -> s) ->
  Int ->
  Code Q (M.Map Int (world -> RuntimeStore)) ->
  Code Q (M.Map Int (world -> RuntimeStore))
addStore extract storeIndex m =
  [||
  M.insert
    storeIndex
    (unsafeCoerce . $$extract)
    $$m
  ||]