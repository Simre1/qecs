{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Qecs.Compile.CreateQuery where

import Control.Applicative
import Control.HigherKindedData
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Coerce (Coercible, coerce)
import Data.Functor.Identity
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (foldl')
import Language.Haskell.TH
import Optics.Core
import Qecs.Bundle
import Qecs.Compile.Environment
import Qecs.Component
import Qecs.ComponentTracker
import Qecs.Entity (Entity)
import Qecs.Simulation
import Qecs.Store.Store (StoreCapabilities (..))

-- data BundleReadfo where
--   BundleReadfo :: Bundle i BundleRead -> Bundle o BundleWrite -> BundleReadfo

-- -- deriving instance Show BundleReadfo

-- newtype QueryCreator = QueryCreator [BundleReadfo] deriving (Semigroup, Monoid)

-- collectQueries :: Simulation a b -> [BundleReadfo]
-- collectQueries simulation = case simulation of
--   SimulationQuery _ queryIn queryOut -> [BundleReadfo queryIn queryOut]
--   SimulationIOQuery _ queryIn queryOut -> [BundleReadfo queryIn queryOut]
--   SimulationSequence s1 s2 -> collectQueries s1 <> collectQueries s2
--   SimulationParallel s1 s2 -> collectQueries s1 <> collectQueries s2
--   _ -> []

data QueryCapabilities = QueryCapabilities

compileQuery ::
  Query Identity a b ->
  CompileM (Code Q (a -> ExecutionM b))
compileQuery query = do
  case query of
    QueryMap f bundleRead bundleWrite -> do
      (IterateF iterateQ _ _) <- iterateBundle bundleRead
      (WriteF writeQ) <- writeBundle bundleWrite
      pure
        [||
        \_ -> do
          iterate <- $$iterateQ
          write <- $$writeQ
          liftIO $ iterate $ \e a -> do
            write e $ runIdentity $ $$f a
        ||]
    QueryMapWithInput f bundleRead bundleWrite -> do
      (IterateF iterateQ _ _) <- iterateBundle bundleRead
      (WriteF writeQ) <- writeBundle bundleWrite
      pure
        [||
        \i -> do
          iterate <- $$iterateQ
          write <- $$writeQ
          liftIO $ iterate $ \e a -> do
            write e $ runIdentity $ $$f i a
        ||]
    QueryFold initialB f bundleRead -> do
      (IterateF iterateQ _ _) <- iterateBundle bundleRead
      pure
        [||
        \_ -> do
          iterate <- $$iterateQ
          ref <- liftIO $ newIORef $$initialB
          liftIO $ iterate $ \_ a -> do
            modifyIORef' ref (\b -> runIdentity ($$f b a))
          liftIO $ readIORef ref
        ||]
    QueryFoldWithInput initialB f bundleRead -> do
      (IterateF iterateQ _ _) <- iterateBundle bundleRead
      pure
        [||
        \i -> do
          iterate <- $$iterateQ
          ref <- liftIO $ newIORef $$initialB
          liftIO $ iterate $ \_ a -> do
            modifyIORef' ref (\b -> runIdentity ($$f b i a))
          liftIO $ readIORef ref
        ||]
    QueryFoldWrite initialB f bundleRead bundleWrite -> do
      (IterateF iterateQ _ _) <- iterateBundle bundleRead
      (WriteF writeQ) <- writeBundle bundleWrite
      pure
        [||
        \_ -> do
          iterate <- $$iterateQ
          write <- $$writeQ
          ref <- liftIO $ newIORef $$initialB
          liftIO $ iterate $ \e a -> do
            b <- readIORef ref
            let (o, !nextB) = runIdentity ($$f b a)
            writeIORef ref nextB
            write e o
          liftIO $ readIORef ref
        ||]
    QueryFoldWithInputAndWrite initialB f bundleRead bundleWrite -> do
      (IterateF iterateQ _ _) <- iterateBundle bundleRead
      (WriteF writeQ) <- writeBundle bundleWrite
      pure
        [||
        \i -> do
          iterate <- $$iterateQ
          write <- $$writeQ
          ref <- liftIO $ newIORef $$initialB
          liftIO $ iterate $ \e a -> do
            b <- readIORef ref
            let (o, !nextB) = runIdentity ($$f b i a)
            writeIORef ref nextB
            write e o
          liftIO $ readIORef ref
        ||]

compileQueryIO ::
  Query IO a b ->
  CompileM (Code Q (a -> ExecutionM b))
compileQueryIO = \case
  QueryMap f bundleRead bundleWrite -> do
    (IterateF iterateQ _ _) <- iterateBundle bundleRead
    (WriteF writeQ) <- writeBundle bundleWrite
    pure
      [||
      \_ -> do
        iterate <- $$iterateQ
        write <- $$writeQ
        liftIO $ iterate $ \e a -> do
          $$f a >>= write e
      ||]
  QueryMapWithInput f bundleRead bundleWrite -> do
    (IterateF iterateQ _ _) <- iterateBundle bundleRead
    (WriteF writeQ) <- writeBundle bundleWrite
    pure
      [||
      \i -> do
        iterate <- $$iterateQ
        write <- $$writeQ
        liftIO $ iterate $ \e a -> do
          $$f i a >>= write e
      ||]
  QueryFold initialB f bundleRead -> do
    (IterateF iterateQ _ _) <- iterateBundle bundleRead
    pure
      [||
      \_ -> do
        iterate <- $$iterateQ
        ref <- liftIO $ newIORef $$initialB
        liftIO $ iterate $ \_ a -> do
          b <- readIORef ref
          !nextB <- $$f b a
          writeIORef ref nextB
        liftIO $ readIORef ref
      ||]
  QueryFoldWithInput initialB f bundleRead -> do
    (IterateF iterateQ _ _) <- iterateBundle bundleRead
    pure
      [||
      \i -> do
        iterate <- $$iterateQ
        ref <- liftIO $ newIORef $$initialB
        liftIO $ iterate $ \_ a -> do
          b <- readIORef ref
          !nextB <- $$f b i a
          writeIORef ref nextB
        liftIO $ readIORef ref
      ||]
  QueryFoldWrite initialB f bundleRead bundleWrite -> do
    (IterateF iterateQ _ _) <- iterateBundle bundleRead
    (WriteF writeQ) <- writeBundle bundleWrite
    pure
      [||
      \_ -> do
        iterate <- $$iterateQ
        write <- $$writeQ
        ref <- liftIO $ newIORef $$initialB
        liftIO $ iterate $ \e a -> do
          b <- readIORef ref
          (o, !nextB) <- $$f b a
          writeIORef ref nextB
          write e o
        liftIO $ readIORef ref
      ||]
  QueryFoldWithInputAndWrite initialB f bundleRead bundleWrite -> do
    (IterateF iterateQ _ _) <- iterateBundle bundleRead
    (WriteF writeQ) <- writeBundle bundleWrite
    pure
      [||
      \i -> do
        iterate <- $$iterateQ
        write <- $$writeQ
        ref <- liftIO $ newIORef $$initialB
        liftIO $ iterate $ \e a -> do
          b <- readIORef ref
          (o, !nextB) <- $$f b i a
          writeIORef ref nextB
          write e o
        liftIO $ readIORef ref
      ||]

newtype ReadF a = ReadF (Code Q (ExecutionM (Entity -> IO a)))

newtype WriteF a = WriteF (Code Q (ExecutionM (Entity -> a -> IO ())))

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
          RuntimeStoreAndCapabilities storeCapabilities getStore <- getStoreForComponent c
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
          RuntimeStoreAndCapabilities storeCapabilities getStore <- getStoreForComponent c
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

iterateBundle :: Bundle i BundleRead -> CompileM (IterateF b i)
iterateBundle bundle = do
  entityComponentsQ <- entityComponentsFromBundle bundle
  containsEntityComponentsQ <- containsEntityComponents
  bundleIterate <-
    htraverse
      ( \(BundleRead c) -> do
          RuntimeStoreAndCapabilities storeCapabilities getStore <- getStoreForComponent c
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
  pure $
    [||
    \componentTracker e entityComponents ->
      $$(componentTrackerCode ^. #writeEntityComponents)
        componentTracker
        e
        entityComponents
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