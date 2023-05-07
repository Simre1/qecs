{-# LANGUAGE ImpredicativeTypes #-}

module Qecs.Compile.Environment where

import Control.HigherKindedData
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.List (sortOn)
import Data.Map qualified as M
import Data.Vector qualified as V
import Data.Word (Word64)
import GHC.Generics (Generic)
import Language.Haskell.TH
import Optics.Core
import Qecs.Component
  ( Component (..),
    ComponentId (..),
    SomeComponent (SomeComponent),
  )
import Qecs.Entity
import Qecs.Store.Store
import Unsafe.Coerce

data RuntimeStoreAndCapabilities a = forall s. RuntimeStoreAndCapabilities (StoreCapabilities s a) (Code Q (ExecutionM (s a)))

runtimeStore :: RuntimeStore
runtimeStore = error "Runtime store was accessed without initialisation"

getStoreForComponent :: Component a -> CompileM (RuntimeStoreAndCapabilities a)
getStoreForComponent component@(Component tr) = do
  activeStores <- view #componentStores <$> askCompileEnvironment
  pure $ case M.lookup (SomeComponent component) activeStores of
    Nothing -> error $ "Store for component " <> show tr <> " is not available."
    Just (componentId, SomeStoreCapabilities storeCapabilities) ->
      RuntimeStoreAndCapabilities (unsafeCoerce storeCapabilities) (getComponentStore componentId)
  where
    getComponentStore :: ComponentId -> Code Q (ExecutionM s)
    getComponentStore (ComponentId componentId) =
      [||
      do
        runtimeStores <- getRuntimeStores
        pure $ unsafeCoerce (runtimeStores V.! componentId)
      ||]

data WorldEnvironment = WorldEnvironment
  { runtimeStores :: !(V.Vector RuntimeStore),
    entityStore :: !(Entities Word64)
  }
  deriving (Generic)

getRuntimeStores :: ExecutionM (V.Vector RuntimeStore)
getRuntimeStores = view #runtimeStores <$> askWorldEnvironment

getEntities :: ExecutionM (Entities Word64)
getEntities = view #entityStore <$> askWorldEnvironment

data CompileState = CompileState
  deriving (Generic)

data CompileEnvironment = CompileEnvironment
  { componentStores :: M.Map SomeComponent (ComponentId, SomeStoreCapabilities)
  }
  deriving (Generic)

newtype CompileM a = CompileM (ReaderT CompileEnvironment (State CompileState) a)
  deriving (Functor, Applicative, Monad)

runCompileM :: CompileEnvironment -> CompileState -> CompileM a -> (a, CompileState)
runCompileM e s (CompileM reader) = runState (runReaderT reader e) s

getCompileState :: CompileM CompileState
getCompileState = CompileM $ ReaderT $ const get

putCompileState :: CompileState -> CompileM ()
putCompileState s = CompileM $ ReaderT $ const $ put s

modifyCompileState :: (CompileState -> CompileState) -> CompileM ()
modifyCompileState f = CompileM $ ReaderT $ const $ modify f

askCompileEnvironment :: CompileM (CompileEnvironment)
askCompileEnvironment = CompileM ask

localCompileEnvironment :: (CompileEnvironment -> CompileEnvironment) -> CompileM a -> CompileM a
localCompileEnvironment f (CompileM action) = CompileM $ local f action

newtype ExecutionM a = ExecutionM (ReaderT WorldEnvironment IO a) deriving (Functor, Applicative, Monad, MonadIO)

askWorldEnvironment :: ExecutionM WorldEnvironment
askWorldEnvironment = ExecutionM ask

runExecutionM :: WorldEnvironment -> ExecutionM a -> IO a
runExecutionM env (ExecutionM action) = runReaderT action env

generateWorldEnvironment :: (HTraversable w, MonadIO m) => w (Store m) -> Code Q (m WorldEnvironment)
generateWorldEnvironment world =
  let storesQ =
        fmap snd $
          sortOn fst $
            M.elems $
              traverseFold
                ( \store@(Store sc create) stores ->
                    M.insert
                      (SomeComponent $ sc ^. #component)
                      ( ComponentId $
                          M.size stores,
                        makeRuntimeStore store
                      )
                      stores
                )
                M.empty
                world
      storeExpressions = map unTypeCode storesQ
      liftedStores =
        unsafeCodeCoerce $
          foldr (\a b -> [|(:) <$> $a <*> $b|]) [|pure []|] storeExpressions
   in [||
      do
        createdStores <- $$liftedStores
        WorldEnvironment (V.fromList createdStores)
          <$> liftIO
            (createEntities 50000)
      ||]



generateCompileEnvironment :: (HTraversable w) => w (Store f) -> CompileEnvironment
generateCompileEnvironment =
  CompileEnvironment
    . traverseFold
      ( \(Store sc _) stores ->
          M.insert
            (SomeComponent $ sc ^. #component)
            ( ComponentId $
                M.size stores,
              SomeStoreCapabilities sc
            )
            stores
      )
      M.empty