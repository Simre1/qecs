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
import Qecs.Compile.Environment
import Qecs.Component
import Qecs.ComponentTracker
import Qecs.Entity (Entity)
import Qecs.Simulation
import Qecs.Store.Store (StoreCapabilities (..), Store (..))
import Qecs.ExecutionM
import Qecs.Query
import Data.Bundle
import Qecs.Compile.BundleOperations

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