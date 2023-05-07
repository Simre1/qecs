import BenchSimulations
import Control.Monad
import Data.Proxy
import Gauge.Main
import Language.Haskell.TH
import Qecs.Compile.Compile (compile, viewCode)
import Qecs.Store.Map
import Qecs.Store.SparseSet
import Qecs.Store.Store

benchMapStore :: IO (Bench Int)
benchMapStore = $$(makeSimulationBench $ World mapStore mapStore)

benchSparseSet :: IO (Bench Int)
benchSparseSet = $$(makeSimulationBench $ World sparseSetStore sparseSetStore)

benchSparseSetStorable :: IO (Bench Int)
benchSparseSetStorable =
  $$( makeSimulationBench $
        World
          (sparseSetStorableStore [||Proxy @Position||])
          (sparseSetStorableStore [||Proxy @Velocity||])
    )

main :: IO ()
main = do
  defaultMain
    [ bgroup
        "cmap"
        [ bench "MapStore" $ perRunEnv benchMapStore (\(Bench action) -> action),
          bench "SparseSet" $ perRunEnv benchSparseSet (\(Bench action) -> action),
          bench "SparseSetStorable" $ perRunEnv benchSparseSetStorable (\(Bench action) -> action)
        ]
    ]
