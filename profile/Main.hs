import Control.Monad
import Data.Proxy
import ProfileSimulations
import Qecs.Compile.Compile (compile)
import Qecs.Store.SparseSet

cmapMapStoreProgram :: IO ()
cmapMapStoreProgram = do
  let (computeState, run) =
        $$( compile
              ( World
                  (sparseSetStorableStore [||Proxy @Position||])
                  (sparseSetStorableStore [||Proxy @Velocity||])
              )
              mapStoreSimulation
          )
  !state <- computeState
  v <- run state ()
  print v

main :: IO ()
main = do
  cmapMapStoreProgram
