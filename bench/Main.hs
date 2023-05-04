import BenchSimulations
import Control.Monad
import Gauge.Main
import Qecs.Compile.Compile (compile, viewCode)
import Qecs.Store.Map
import Qecs.Store.SparseSet

prepareMapStoreWorld :: IO MapStores
prepareMapStoreWorld = do
  !store1 <- mapStore
  !store2 <- mapStore

  let (computeState, run) = $$(compile allocateEntitiesMapStoreBench)

  !state <- computeState $ MapStores store1 store2
  run state ()
  pure $ MapStores store1 store2

cmapBenchMapStoreProgram :: MapStores -> IO (IO Int)
cmapBenchMapStoreProgram world = do
  let (computeState, run) = $$(compile cmapMapStoreBench)
  !state <- computeState world
  pure $ run state ()

prepareSparseSetWorld :: IO SparseSets
prepareSparseSetWorld = do
  store1 <- sparseSetStore
  store2 <- sparseSetStore

  let (computeState, run) = $$(compile allocateEntitiesSparseSetBench)

  state <- computeState $ SparseSets store1 store2
  run state ()
  pure $ SparseSets store1 store2

cmapBenchSparseSetProgram :: SparseSets -> IO (IO Int)
cmapBenchSparseSetProgram world = do
  let (computeState, run) = $$(compile cmapSparseSetBench)
  !state <- computeState world
  pure $ run state ()

prepareSparseSetStorableWorld :: IO SparseSetsStorable
prepareSparseSetStorableWorld = do
  store1 <- sparseSetStorableStore
  store2 <- sparseSetStorableStore

  let (computeState, run) = $$(compile allocateEntitiesSparseSetStorableBench)

  state <- computeState $ SparseSetsStorable store1 store2
  run state ()
  pure $ SparseSetsStorable store1 store2

cmapBenchSparseSetStorableProgram :: SparseSetsStorable -> IO (IO Int)
cmapBenchSparseSetStorableProgram world = do
  let (computeState, run) = $$(compile cmapSparseSetStorableBench)
  state <- computeState world
  pure $ run state ()

main :: IO ()
main = do
  defaultMain
    [ bgroup
        "cmap"
        [ bench "MapStore" $ perRunEnv prepareMapStoreWorld $ \world -> join $ cmapBenchMapStoreProgram world,
          bench "SparseSetBoxed" $ perRunEnv prepareSparseSetWorld $ \world -> join $ cmapBenchSparseSetProgram world,
          bench "SparseSetStorable" $ perRunEnv prepareSparseSetStorableWorld $ \world -> join $ cmapBenchSparseSetStorableProgram world
        ]
    ]
