module Qecs.Compile.BundleOperations where

import Control.Applicative
import Control.HigherKindedData
import Control.Monad
import Control.Monad.IO.Class
import Data.Bundle
import Data.Coerce
import Data.Functor.Identity
import Language.Haskell.TH
import Optics.Core
import Qecs.Compile.Environment
import Qecs.Component
import Qecs.ComponentTracker
import Qecs.Entity
import Qecs.ExecutionM
import Qecs.Query
import Qecs.Store.Store

newtype ReadF a = ReadF (Code Q (ExecutionM (Entity -> IO a)))

newtype WriteF a = WriteF (Code Q (ExecutionM (Entity -> a -> IO ())))

newtype DeleteF a = DeleteF (Code Q (ExecutionM (Entity -> IO ())))

data IterateF b a = IterateF
  { iterate :: Code Q (ExecutionM ((Entity -> a -> IO ()) -> IO ())),
    members :: Code Q (ExecutionM Int),
    read :: Code Q (ExecutionM (Entity -> IO a))
  }

readBundle :: Bundle i BundleRead -> CompileM (ReadF i)
readBundle bundle = do
  bundleRead <-
    htraverse
      ( \(BundleRead c) -> do
          (Store storeCapabilities getStore) <- getStoreForComponent c
          pure $
            ReadF
              [||
              do
                s <- $$getStore
                pure $ \entity -> liftIO $ $$(storeCapabilities ^. #read) s entity
              ||]
      )
      bundle
  pure $
    hfold
      ( \combine (ReadF readX) (ReadF readY) ->
          ReadF
            [||
            do
              readX' <- $$readX
              readY' <- $$readY
              pure $ \e -> curry $$(combine ^. #co) <$> readX' e <*> readY' e
            ||]
      )
      (ReadF [||pure (\_ -> pure ())||])
      bundleRead

writeBundle :: Bundle i BundleWrite -> CompileM (WriteF i)
writeBundle bundle = do
  entityComponentsQ <- entityComponentsFromBundle bundle
  addEntityComponentsQ <- addEntityComponents
  bundleWrite <-
    htraverse
      ( \(BundleWrite c) -> do
          (Store storeCapabilities getStore) <- getStoreForComponent c
          pure $
            WriteF
              [||
              do
                s <- $$getStore
                componentTracker <- getComponentTracker
                let entityComponents = $$entityComponentsQ
                pure $ \entity v -> liftIO $ do
                  $$(storeCapabilities ^. #write) s entity v
                  $$addEntityComponentsQ componentTracker entity entityComponents
              ||]
      )
      bundle
  pure $
    hfold
      ( \combine (WriteF writeQX) (WriteF writeQY) ->
          WriteF
            [||
            do
              writeX <- $$writeQX
              writeY <- $$writeQY
              pure $ \e z ->
                let (x, y) = $$(combine ^. #contra) z
                 in writeX e x *> writeY e y
            ||]
      )
      (WriteF [||pure $ \_ _ -> pure ()||])
      bundleWrite

deleteBundle :: Bundle i BundleDelete -> CompileM (DeleteF i)
deleteBundle bundle = do
  bundleDelete <-
    htraverse
      ( \(BundleDelete c) -> do
          (Store storeCapabilities getStore) <- getStoreForComponent c
          pure $
            DeleteF
              [||
              do
                s <- $$getStore
                pure $ \entity -> liftIO $ do
                  $$(storeCapabilities ^. #delete) s entity
              ||]
      )
      bundle
  pure $
    hfold @Identity
      ( \_ (DeleteF deleteQX) (DeleteF deleteQY) ->
          DeleteF
            [||
            do
              deleteX <- $$deleteQX
              deleteY <- $$deleteQY
              pure $ \e -> deleteX e *> deleteY e
            ||]
      )
      (DeleteF [||pure $ \_ -> pure ()||])
      bundleDelete

iterateBundle :: Bundle i BundleRead -> CompileM (IterateF b i)
iterateBundle bundle = do
  entityComponentsQ <- entityComponentsFromBundle bundle
  containsEntityComponentsQ <- containsEntityComponents
  bundleIterate <-
    htraverse
      ( \(BundleRead c) -> do
          (Store storeCapabilities getStore) <- getStoreForComponent c
          let iterateQ = storeCapabilities ^. #iterate
          pure $
            IterateF
              [||
              do
                s <- $$getStore
                pure $ \f -> liftIO $ $$iterateQ s f
              ||]
              [||$$getStore >>= liftIO . $$(storeCapabilities ^. #members)||]
              [||
              do
                s <- $$getStore
                pure $ \entity -> liftIO $ $$(storeCapabilities ^. #read) s entity
              ||]
      )
      bundle
  pure $
    hfold
      ( \combine (IterateF iterateQX membersQX readQX) (IterateF iterateQY membersQY readQY) ->
          IterateF
            [||
            do
              membersX <- $$membersQX
              membersY <- $$membersQY
              componentTracker <- getComponentTracker
              if membersX < membersY
                then do
                  iterateX' <- $$iterateQX
                  readY <- $$readQY
                  pure $ \f ->
                    iterateX' $ \e xValues -> do
                      yContained <- $$containsEntityComponentsQ componentTracker e $$entityComponentsQ
                      when yContained $ do
                        yValues <- liftIO $ readY e
                        let zValues = $$(combine ^. #co) (xValues, yValues)
                        f e zValues
                else do
                  iterateY <- $$iterateQY
                  readX <- $$readQX
                  pure $ \f ->
                    iterateY $ \e yValues -> do
                      xContained <- $$containsEntityComponentsQ componentTracker e $$entityComponentsQ
                      when xContained $ do
                        xValues <- liftIO $ readX e
                        let zValues = $$(combine ^. #co) (xValues, yValues)
                        f e zValues
            ||]
            [||
            do
              min <$> $$membersQX <*> $$membersQY
            ||]
            [||
            do
              readX <- $$readQX
              readY <- $$readQY
              pure $ \e -> curry $$(combine ^. #co) <$> readX e <*> readY e
            ||]
      )
      ( IterateF
          [||pure $ \_ -> pure ()||]
          [||pure maxBound||]
          [||pure $ \_ -> pure ()||]
      )
      bundleIterate

componentIdsFromBundle :: Bundle b Component -> CompileM [ComponentId]
componentIdsFromBundle bundle = do
  componentIdBundle <- htraverse toComponentId bundle
  pure $
    getConst $
      hfold @Identity
        (\_ (Const componentsA) (Const componentsB) -> Const $ componentsA <> componentsB)
        (Const [])
        componentIdBundle
  where
    toComponentId :: Component a -> CompileM (Const [ComponentId] a)
    toComponentId component = Const . pure <$> getComponentId component

entityComponentsFromBundle :: (Coercible (Bundle b f) (Bundle b Component)) => Bundle b f -> CompileM (Code Q EntityComponents)
entityComponentsFromBundle bundle = do
  componentTrackerCode <- view #componentTrackerCode <$> askCompileEnvironment
  componentIds <- componentIdsFromBundle $ coerce bundle
  pure $ componentIdsToEntityComponents componentTrackerCode componentIds

addEntityComponents :: CompileM (Code Q (ComponentTracker -> Entity -> EntityComponents -> IO ()))
addEntityComponents = do
  componentTrackerCode <- view #componentTrackerCode <$> askCompileEnvironment
  pure $
    [||
    \componentTracker e entityComponents ->
      $$(componentTrackerCode ^. #modifyEntityComponents)
        componentTracker
        e
        ($$(componentTrackerCode ^. #orComponents) entityComponents)
    ||]

writeEntityComponents :: CompileM (Code Q (ComponentTracker -> Entity -> EntityComponents -> IO ()))
writeEntityComponents = do
  componentTrackerCode <- view #componentTrackerCode <$> askCompileEnvironment
  pure
    [||
    $$(componentTrackerCode ^. #writeEntityComponents)
    ||]

containsEntityComponents :: CompileM (Code Q (ComponentTracker -> Entity -> EntityComponents -> IO Bool))
containsEntityComponents = do
  componentTrackerCode <- view #componentTrackerCode <$> askCompileEnvironment
  pure $
    [||
    \componentTracker e entityComponents ->
      $$(componentTrackerCode ^. #containsComponents)
        entityComponents
        <$> $$(componentTrackerCode ^. #readEntityComponents) componentTracker e
    ||]

getComponentIds :: CompileM (Code Q (ComponentTracker -> Entity -> IO [ComponentId]))
getComponentIds = do
  componentTrackerCode <- view #componentTrackerCode <$> askCompileEnvironment
  pure
    [||
    \componentTracker e -> do
      entityComponents <- $$(componentTrackerCode ^. #readEntityComponents) componentTracker e
      pure $ $$(componentTrackerCode ^. #getComponentIds) entityComponents
    ||]