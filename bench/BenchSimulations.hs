{-# LANGUAGE DerivingVia #-}

module BenchSimulations where

import Control.DeepSeq
import Control.HigherKindedData
import Data.Foldable (Foldable (..))
import Data.Monoid
import Foreign (Storable (..), castPtr, plusPtr)
import GHC.Generics
import Language.Haskell.TH
import Qecs.Compile.Compile
import Qecs.Simulation
import Qecs.Store.Store
import Qecs.Compile.Environment (WorldEnvironment(WorldEnvironment))

data World f = World
  { position :: f Position,
    velocity :: f Velocity
  }
  deriving (Generic)

instance HTraversable World where
  htraverse f (World p v) = World <$> f p <*> f v

data Position = Position !Int !Int deriving (Show)

instance Storable Position where
  sizeOf _ = 2 * sizeOf (undefined :: Int)
  alignment _ = alignment (undefined :: Int)
  peek ptr = do
    let p = castPtr ptr
    v1 <- peek p
    v2 <- peek (p `plusPtr` sizeOf (undefined :: Int))
    pure $ Position v1 v2
  poke ptr (Position v1 v2) = do
    let p = castPtr ptr
    poke p v1
    poke (p `plusPtr` sizeOf (undefined :: Int)) v2

  {-# INLINE sizeOf #-}
  {-# INLINE alignment #-}
  {-# INLINE poke #-}
  {-# INLINE peek #-}

data Velocity = Velocity !Int !Int deriving (Show)

instance Storable Velocity where
  sizeOf _ = 2 * sizeOf (undefined :: Int)
  alignment _ = alignment (undefined :: Int)
  peek ptr = do
    let p = castPtr ptr
    v1 <- peek p
    v2 <- peek (p `plusPtr` sizeOf (undefined :: Int))
    pure $ Velocity v1 v2
  poke ptr (Velocity v1 v2) = do
    let p = castPtr ptr
    poke p v1
    poke (p `plusPtr` sizeOf (undefined :: Int)) v2

  {-# INLINE sizeOf #-}
  {-# INLINE alignment #-}
  {-# INLINE poke #-}
  {-# INLINE peek #-}

allocateEntities :: Simulation () ()
allocateEntities =
  spure [||replicate 1000 (Position 0 0, Velocity 1 1)||]
    >>> makeEntities
    >>> spure [||replicate 9000 (Position 0 0)||]
    >>> makeEntities
    >>> SimulationPure [||()||]

applyVelocity :: Simulation () Int
applyVelocity =
  foldl' (\b _ -> b >>> f) f [1 .. 5]
    >>> cfold [||(\(Position a b) -> Sum a + Sum b)||]
    >>> SimulationArr [||getSum||]
  where
    f :: Simulation a ()
    f = cmap [||\(Position a b, Velocity da db) -> Position (a + da) (b + db)||]

newtype Bench a = Bench (IO Int)

instance NFData (Bench a) where
  rnf (Bench _) = ()

makeSimulationBench :: World (Store IO) -> Code Q (IO (Bench Int))
makeSimulationBench world =
  [||
  do
    let (computeWorldEnvironment, alloc) =
          $$(compile @_ @IO world allocateEntities)
    let (_, run) = $$(compile world applyVelocity)
    worldEnvironment <- computeWorldEnvironment
    alloc worldEnvironment ()
    pure $ Bench $ run worldEnvironment ()
  ||]