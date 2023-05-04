import ProfileSimulations
import Control.Monad
import Qecs.Compile.Compile (compile, viewCode)
import Qecs.Store.Map
import Qecs.Store.SparseSet

cmapMapStoreProgram :: IO ()
cmapMapStoreProgram = do
  !store1 <- sparseSetStorableStore
  !store2 <- sparseSetStorableStore
  let (computeState, run) = $$(compile mapStoreSimulation)
  !state <- computeState $ World store1 store2
  v <- run state ()
  print v


main :: IO ()
main = do
  cmapMapStoreProgram
