{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Qecs.Compile.CreateQuery where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Qecs.Bundle
import Qecs.Compile.Environment
import Qecs.Entity (Entity)
import Qecs.Store.Store (StoreCapabilities (..), StoreIterate (..))
import Language.Haskell.TH
import Optics.Core

data BundleReadfo where
  BundleReadfo :: Bundle BundleRead i -> Bundle BundleWrite o -> BundleReadfo

-- deriving instance Show BundleReadfo

newtype QueryCreator = QueryCreator [BundleReadfo] deriving (Semigroup, Monoid)

-- collectQueries :: Simulation a b -> [BundleReadfo]
-- collectQueries simulation = case simulation of
--   SimulationQuery _ queryIn queryOut -> [BundleReadfo queryIn queryOut]
--   SimulationIOQuery _ queryIn queryOut -> [BundleReadfo queryIn queryOut]
--   SimulationSequence s1 s2 -> collectQueries s1 <> collectQueries s2
--   SimulationParallel s1 s2 -> collectQueries s1 <> collectQueries s2
--   _ -> []

data QueryCapabilities = QueryCapabilities

compileQuery ::
  Code Q (b -> x -> b) ->
  Code Q b ->
  Code Q (a -> i -> (o, x)) ->
  Bundle BundleRead i ->
  Bundle BundleWrite o ->
  CompileM world (Code Q (a -> ExecutionM b))
compileQuery accumulate acc queryF bundleIn bundleOut = do
  (IterateF iterate _ _) <- iterateBundle bundleIn
  (WriteF write) <- writeBundle bundleOut
  pure
    [||
    \i -> do
      iterate' <- $$iterate
      write' <- $$write
      liftIO $ iterate' $$acc $ \b e v -> do
        let (o, x) = $$queryF i v
        write' e o
        pure $ $$accumulate b x
    ||]

compileIOQuery ::
  Code Q (b -> x -> b) ->
  Code Q b ->
  Code Q (a -> i -> IO (o, x)) ->
  Bundle BundleRead i ->
  Bundle BundleWrite o ->
  CompileM world (Code Q (a -> ExecutionM b))
compileIOQuery accumulate acc queryF bundleIn bundleOut = do
  (IterateF iterate _ _) <- iterateBundle bundleIn
  (WriteF write) <- writeBundle bundleOut
  pure
    [||
    \i -> do
      iterate' <- $$iterate
      write' <- $$write
      liftIO $ iterate' $$acc $ \b e v -> do
        (o, x) <- $$queryF i v
        write' e o
        pure $ $$accumulate b x
    ||]

newtype ReadF a = ReadF (Code Q (ExecutionM (Entity -> IO a)))

newtype WriteF a = WriteF (Code Q (ExecutionM (Entity -> a -> IO ())))

data IterateF b a = IterateF
  { iterate :: Code Q (ExecutionM (b -> (b -> Entity -> a -> IO b) -> IO b)),
    members :: Code Q (ExecutionM Int),
    read :: Code Q (ExecutionM (Entity -> IO a))
  }

readBundle :: Bundle BundleRead i -> CompileM w (ReadF i)
readBundle bundle = do
  bundleRead <-
    bundleTraverseF
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
    bundleFoldF
      ( \combine (ReadF readX) (ReadF readY) ->
          ReadF
            [||
            do
              readX' <- $$readX
              readY' <- $$readY
              pure $ \e -> curry $$(combine ^. #coFunction) <$> readX' e <*> readY' e
            ||]
      )
      (ReadF [||pure (\_ -> pure ())||])
      bundleRead

writeBundle :: Bundle BundleWrite i -> CompileM w (WriteF i)
writeBundle bundle = do
  bundleWrite <-
    bundleTraverseF
      ( \(BundleWrite c) -> do
          RuntimeStoreAndCapabilities storeCapabilities getStore <- getStoreForComponent c
          pure $
            WriteF
              [||
              do
                s <- $$getStore
                pure $ \entity v ->
                  liftIO $ $$(storeCapabilities ^. #write) s entity v
              ||]
      )
      bundle
  pure $
    bundleFoldF
      ( \combine (WriteF writeX) (WriteF writeY) ->
          WriteF
            [||
            do
              writeX' <- $$writeX
              writeY' <- $$writeY
              pure $ \e z ->
                let (x, y) = $$(combine ^. #contraFunction) z
                 in writeX' e x *> writeY' e y
            ||]
      )
      (WriteF [||pure $ \_ _ -> pure ()||])
      bundleWrite

iterateBundle :: Bundle BundleRead i -> CompileM w (IterateF b i)
iterateBundle bundle = do
  bundleIterate <-
    bundleTraverseF
      ( \(BundleRead c) -> do
          RuntimeStoreAndCapabilities storeCapabilities getStore <- getStoreForComponent c
          let StoreIterate iterate = storeCapabilities ^. #iterate
          pure $
            IterateF
              [||
              do
                s <- $$getStore
                pure $ \b f -> liftIO $ $$iterate s b f
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
    bundleFoldF
      ( \combine (IterateF iterateX membersX readX) (IterateF iterateY membersY readY) ->
          IterateF
            [||
            do
              membersX' <- $$membersX
              membersY' <- $$membersY
              if membersX' < membersY'
                then do
                  iterateX' <- $$iterateX
                  readY' <- $$readY
                  pure $ \initialB f ->
                    iterateX' initialB $ \b e xValues -> do
                      yValues <- liftIO $ readY' e
                      let zValues = $$(combine ^. #coFunction) (xValues, yValues)
                      f b e zValues
                else do
                  iterateY' <- $$iterateY
                  readX' <- $$readX
                  pure $ \initialB f ->
                    iterateY' initialB $ \b e yValues -> do
                      xValues <- liftIO $ readX' e
                      let zValues = $$(combine ^. #coFunction) (xValues, yValues)
                      f b e zValues
            ||]
            [||
            do
              min <$> $$membersX <*> $$membersY
            ||]
            [||
            do
              readX' <- $$readX
              readY' <- $$readY
              pure $ \e -> curry $$(combine ^. #coFunction) <$> readX' e <*> readY' e
            ||]
      )
      ( IterateF
          [||pure $ \initialB _ -> pure initialB||]
          [||pure maxBound||]
          [||pure $ \_ -> pure ()||]
      )
      bundleIterate