module Qecs.Compile.MyQ where
import Control.Monad.IO.Class (MonadIO)
import Language.Haskell.TH
import Control.Monad.Trans.State


newtype MyQ a = MyQ (StateT CompileState Q a) deriving (Functor, Applicative, Monad, MonadIO)

data CompileState = CompileState

instance Quote MyQ where
  newName str = MyQ $ StateT $ \s -> (,s) <$> newName str


