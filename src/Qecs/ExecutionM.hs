module Qecs.ExecutionM where
import Qecs.Store.Store
import qualified Data.Vector as V
import Qecs.Entity
import Qecs.ComponentTracker
import GHC.Generics
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Optics.Core

data WorldEnvironment = WorldEnvironment
  { runtimeStores :: !(V.Vector RuntimeStore),
    entityStore :: !Entities,
    componentTracker :: ComponentTracker
  }
  deriving (Generic)

getRuntimeStores :: ExecutionM (V.Vector RuntimeStore)
getRuntimeStores = view #runtimeStores <$> askWorldEnvironment

getEntities :: ExecutionM Entities
getEntities = view #entityStore <$> askWorldEnvironment

getComponentTracker :: ExecutionM ComponentTracker
getComponentTracker = view #componentTracker <$> askWorldEnvironment

newtype ExecutionM a = ExecutionM (ReaderT WorldEnvironment IO a) deriving (Functor, Applicative, Monad, MonadIO)

askWorldEnvironment :: ExecutionM WorldEnvironment
askWorldEnvironment = ExecutionM ask

runExecutionM :: WorldEnvironment -> ExecutionM a -> IO a
runExecutionM env (ExecutionM action) = runReaderT action env