{-# LANGUAGE ImpredicativeTypes #-}

module Qecs.Compile.Environment where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Map qualified as M
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Qecs.Component
import Qecs.Entity
import Qecs.Store.Store
import Language.Haskell.TH
import Optics.Core
import Unsafe.Coerce

data RuntimeStoreAndCapabilities a = forall s. RuntimeStoreAndCapabilities (StoreCapabilities s a) (Code Q (ExecutionM s))

data RuntimeStore

runtimeStore :: RuntimeStore
runtimeStore = error "Runtime store was accessed without initialisation"

getStoreForComponent :: Component a -> CompileM world (RuntimeStoreAndCapabilities a)
getStoreForComponent component@(Component tr) = do
  activeStores <- view #activeStores <$> askCompileEnvironment
  pure $ case M.lookup (SomeComponent component) activeStores of
    Nothing -> error $ "Store for component " <> show tr <> " is not available."
    Just (componentId, SomeStoreCapabilities storeCapabilities) ->
      RuntimeStoreAndCapabilities (unsafeCoerce storeCapabilities) (getComponentStore componentId)
  where
    getComponentStore :: Int -> Code Q (ExecutionM s)
    getComponentStore componentId =
      [||
      do
        runtimeStores <- getRuntimeStores
        pure $ unsafeCoerce (runtimeStores V.! componentId)
      ||]

data WorldEnvironment = WorldEnvironment
  { runtimeStores :: !(V.Vector RuntimeStore),
    entityStore :: !EntityStore
  }
  deriving (Generic)

getRuntimeStores :: ExecutionM (V.Vector RuntimeStore)
getRuntimeStores = view #runtimeStores <$> askWorldEnvironment

getEntityStore :: ExecutionM EntityStore
getEntityStore = view #entityStore <$> askWorldEnvironment

data CompileState world = CompileState
  { nextStoreIndex :: Int,
    neededStores :: Code Q (M.Map Int (world -> RuntimeStore))
  }
  deriving (Generic)

data CompileEnvironment = CompileEnvironment
  { activeStores :: M.Map SomeComponent (Int, SomeStoreCapabilities)
  }
  deriving (Generic)

newtype CompileM world a = CompileM (ReaderT CompileEnvironment (State (CompileState world)) a)
  deriving (Functor, Applicative, Monad)

runCompileM :: CompileEnvironment -> (CompileState world) -> CompileM world a -> (a, (CompileState world))
runCompileM e s (CompileM reader) = runState (runReaderT reader e) s

getCompileState :: CompileM world (CompileState world)
getCompileState = CompileM $ ReaderT $ const get

putCompileState :: CompileState world -> CompileM world ()
putCompileState s = CompileM $ ReaderT $ const $ put s

modifyCompileState :: ((CompileState world) -> (CompileState world)) -> CompileM world ()
modifyCompileState f = CompileM $ ReaderT $ const $ modify f

askCompileEnvironment :: CompileM world CompileEnvironment
askCompileEnvironment = CompileM ask

localCompileEnvironment :: (CompileEnvironment -> CompileEnvironment) -> CompileM world a -> CompileM world a
localCompileEnvironment f (CompileM action) = CompileM $ local f action

newtype ExecutionM a = ExecutionM (ReaderT WorldEnvironment IO a) deriving (Functor, Applicative, Monad, MonadIO)

askWorldEnvironment :: ExecutionM WorldEnvironment
askWorldEnvironment = ExecutionM ask

runExecutionM :: WorldEnvironment -> ExecutionM a -> IO a
runExecutionM env (ExecutionM action) = runReaderT action env
