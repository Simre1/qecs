{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Qecs.Compile.CreateQuery where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Functor.Identity
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Language.Haskell.TH
import Optics.Core
import Qecs.Bundle
import Qecs.Compile.Environment
import Qecs.Entity (Entity)
import Qecs.Simulation
import Qecs.Store.Store (StoreCapabilities (..))

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
  Query Identity a b ->
  CompileM w (Code Q (a -> ExecutionM b))
compileQuery = \case
  QueryMap f bundleRead bundleWrite -> do
    (IterateF iterateQ _ _ _) <- iterateBundle bundleRead
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
    (IterateF iterateQ _ _ _) <- iterateBundle bundleRead
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
    (IterateF iterateQ _ _ _) <- iterateBundle bundleRead
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
    (IterateF iterateQ _ _ _) <- iterateBundle bundleRead
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
    (IterateF iterateQ _ _ _) <- iterateBundle bundleRead
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
    (IterateF iterateQ _ _ _) <- iterateBundle bundleRead
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
  CompileM w (Code Q (a -> ExecutionM b))
compileQueryIO = \case
  QueryMap f bundleRead bundleWrite -> do
    (IterateF iterateQ _ _ _) <- iterateBundle bundleRead
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
    (IterateF iterateQ _ _ _) <- iterateBundle bundleRead
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
    (IterateF iterateQ _ _ _) <- iterateBundle bundleRead
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
    (IterateF iterateQ _ _ _) <- iterateBundle bundleRead
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
    (IterateF iterateQ _ _ _) <- iterateBundle bundleRead
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
    (IterateF iterateQ _ _ _) <- iterateBundle bundleRead
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
    has :: Code Q (ExecutionM (Entity -> IO Bool)),
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
      ( \combine (WriteF writeQX) (WriteF writeQY) ->
          WriteF
            [||
            do
              writeX <- $$writeQX
              writeY <- $$writeQY
              pure $ \e z ->
                let (x, y) = $$(combine ^. #contraFunction) z
                 in writeX e x *> writeY e y
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
                pure $ \entity -> liftIO $ $$(storeCapabilities ^. #has) s entity
              ||]
              [||
              do
                s <- $$getStore
                pure $ \entity -> liftIO $ $$(storeCapabilities ^. #read) s entity
              ||]
      )
      bundle
  pure $
    bundleFoldF
      ( \combine (IterateF iterateQX membersQX hasQX readQX) (IterateF iterateQY membersQY hasQY readQY) ->
          IterateF
            [||
            do
              membersX <- $$membersQX
              membersY <- $$membersQY
              if membersX < membersY
                then do
                  iterateX' <- $$iterateQX
                  hasY <- $$hasQY
                  readY <- $$readQY
                  pure $ \f ->
                    iterateX' $ \e xValues -> do
                      yContained <- hasY e
                      when yContained $ do
                        yValues <- liftIO $ readY e
                        let zValues = $$(combine ^. #coFunction) (xValues, yValues)
                        f e zValues
                else do
                  iterateY <- $$iterateQY
                  hasX <- $$hasQX
                  readX <- $$readQX
                  pure $ \f ->
                    iterateY $ \e yValues -> do
                      xContained <- hasX e
                      when xContained $ do
                        xValues <- liftIO $ readX e
                        let zValues = $$(combine ^. #coFunction) (xValues, yValues)
                        f e zValues
            ||]
            [||
            do
              min <$> $$membersQX <*> $$membersQY
            ||]
            [||
            do
              hasX <- $$hasQX
              hasY <- $$hasQY
              pure $ \e -> (&&) <$> hasX e <*> hasY e
            ||]
            [||
            do
              readX <- $$readQX
              readY <- $$readQY
              pure $ \e -> curry $$(combine ^. #coFunction) <$> readX e <*> readY e
            ||]
      )
      ( IterateF
          [||pure $ \_ -> pure ()||]
          [||pure maxBound||]
          [||pure $ \_ -> pure False||]
          [||pure $ \_ -> pure ()||]
      )
      bundleIterate