module Control.HigherKindedData where

import Control.Monad (void)
import Control.Monad.ST
import Data.STRef
import GHC.Generics

class HFunctor d where
  hmap :: (forall x. f x -> g x) -> d f -> d g

class HTraversable d where
  htraverse :: (Applicative m) => (forall x. f x -> m (g x)) -> d f -> m (d g)

class HEmpty where
  hempty :: d Empty

data Empty a = Empty

htraverse_ :: (HTraversable d, Applicative m) => (forall x. f x -> m y) -> d f -> m ()
htraverse_ f d = void $ htraverse (\x -> x <$ f x) d

traverseFold :: (HTraversable w) => (forall a. f a -> x -> x) -> x -> w f -> x
traverseFold f x d = runST $ do
  ref <- newSTRef x
  htraverse_
    (modifySTRef' ref . f)
    d
  readSTRef ref

data VariableFunction f a b = VariableFunction
  { co :: f (a -> b),
    contra :: f (b -> a)
  }
  deriving (Generic)

class HFoldable g d where
  hfold :: (forall x y z. VariableFunction g (x, y) z -> f x -> f y -> f z) -> f () -> d a f -> f a
