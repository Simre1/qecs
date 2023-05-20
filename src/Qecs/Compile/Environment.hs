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
import Qecs.ComponentTracker
import Qecs.Entity
import Qecs.ExecutionM
import Qecs.Store.Store
import Unsafe.Coerce

runtimeStore :: RuntimeStore
runtimeStore = error "Runtime store was accessed without initialisation"

getStoreForComponent :: Component a -> CompileM (Store ExecutionM a)
getStoreForComponent component@(Component tr) = do
  componentIds <- view #componentIds <$> askCompileEnvironment
  case M.lookup (SomeComponent component) componentIds of
    Nothing -> error $ "Store for component " <> show tr <> " is not available."
    Just componentId -> getStoreForComponentId componentId

getStoreForComponentId :: ComponentId -> CompileM (Store ExecutionM a)
getStoreForComponentId componentId = do
  componentStores <- view #componentStores <$> askCompileEnvironment
  pure $ case componentStores M.! componentId of
    (SomeStoreCapabilities storeCapabilities) ->
      Store (unsafeCoerce storeCapabilities) (getComponentStore componentId)
  where
    getComponentStore :: ComponentId -> Code Q (ExecutionM s)
    getComponentStore (ComponentId componentId) =
      [||
      do
        runtimeStores <- getRuntimeStores
        pure $ unsafeCoerce (runtimeStores V.! componentId)
      ||]

getComponentId :: Component a -> CompileM ComponentId
getComponentId component@(Component tr) = do
  componentIds <- view #componentIds <$> askCompileEnvironment
  pure $ case M.lookup (SomeComponent component) componentIds of
    Nothing -> error $ "Store for component " <> show tr <> " is not available."
    Just componentId -> componentId

data CompileState = CompileState
  deriving (Generic)

data CompileEnvironment = CompileEnvironment
  { componentIds :: M.Map SomeComponent ComponentId,
    componentStores :: M.Map ComponentId SomeStoreCapabilities,
    componentTrackerCode :: ComponentTrackerCode
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

askCompileEnvironment :: CompileM CompileEnvironment
askCompileEnvironment = CompileM ask

localCompileEnvironment :: (CompileEnvironment -> CompileEnvironment) -> CompileM a -> CompileM a
localCompileEnvironment f (CompileM action) = CompileM $ local f action

generateWorldEnvironment :: (HTraversable w, MonadIO m) => w (Store m) -> CompileM (Code Q (m WorldEnvironment))
generateWorldEnvironment world = do
  compileEnvironment <- askCompileEnvironment
  let storesQ =
        fmap snd $
          sortOn fst $
            M.elems $
              traverseFold
                ( \store@(Store sc _) stores ->
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
  pure
    [||
    do
      createdStores <- $$liftedStores
      WorldEnvironment (V.fromList createdStores)
        <$> liftIO
          (createEntities 50000)
        <*> liftIO
          ($$(compileEnvironment ^. #componentTrackerCode % #create) 50000)
    ||]

generateCompileEnvironment :: (HTraversable w) => w (Store f) -> CompileEnvironment
generateCompileEnvironment world =
  let (ids, stores) =
        traverseFold
          ( \(Store sc _) (ids, stores) ->
              let componentId = ComponentId $ M.size ids
               in ( M.insert
                      (SomeComponent $ sc ^. #component)
                      componentId
                      ids,
                    M.insert
                      componentId
                      ( SomeStoreCapabilities sc
                      )
                      stores
                  )
          )
          (M.empty, M.empty)
          world
      componentTracker = createComponentTrackerCode (M.size stores)
   in CompileEnvironment ids stores componentTracker