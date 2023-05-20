module Qecs.Compile.GetResource where

import Control.HigherKindedData
import Data.Bundle
import Data.Coerce (coerce)
import Data.Functor.Compose
import Data.Functor.Identity
import Language.Haskell.TH
import Optics.Core
import Qecs.Compile.BundleOperations
import Qecs.Compile.Environment
import Qecs.Entity
import Qecs.ExecutionM
import Qecs.Resource

-- getWorldResourceBundle :: Bundle w WorldResource -> CompileM w
-- getWorldResourceBundle bundle = do
--   worldResourceBundle <- htraverse getWorldResource bundle
--   let (Identity resources) =
--         hfold
--           ( \combine a b ->
--               curry <$> (combine ^. #co) <*> a <*> b
--           )
--           (Identity ())
--           worldResourceBundle
--   pure resources
--   where
--     getWorldResource :: WorldResource a -> CompileM (Identity a)
--     getWorldResource wr =
--       Identity <$> case wr of
--         StoreResource component -> getStoreForComponent component

getResourceBundle :: Bundle rs Resource -> CompileM (Code Q (ExecutionM rs))
getResourceBundle resourceBundle = do
  rsBundle <- htraverse buildResourceFromWorld resourceBundle
  let (Compose resources) =
        hfold
          ( \combine (Compose a) (Compose b) ->
              Compose [||curry $$(combine ^. #co) <$> $$a <*> $$b||]
          )
          (Compose [||pure ()||])
          rsBundle
  pure resources
  where
    buildResourceFromWorld :: Resource a -> CompileM (Compose (Code Q) ExecutionM a)
    buildResourceFromWorld (ResourceGet bundle) = do
      ReadF read <- readBundle bundle
      pure $ Compose [||Get <$> $$read||]
    buildResourceFromWorld (ResourceNew bundle) = do
      WriteF writeQ <- writeBundle bundle
      pure $
        Compose
          [||
          do
            entities <- getEntities
            write <- $$writeQ
            pure $ New $ \values -> do
              entity <- createEntity entities
              write entity values
              pure entity
          ||]
    buildResourceFromWorld (ResourceWrite bundle) = do
      WriteF writeQ <- writeBundle bundle
      pure $
        Compose
          [||
          do
            write <- $$writeQ
            pure $ Put $ \entity values -> do
              write entity values
          ||]
    buildResourceFromWorld (ResourceDelete bundle) = do
      DeleteF deleteQ <- deleteBundle bundle
      pure $
        Compose
          [||
          do
            delete <- $$deleteQ
            pure $ Delete $ \entity -> do
              delete entity
          ||]
    buildResourceFromWorld ResourceDeleteEntity = do
        error "Not yet implemented since its pretty hard"
        