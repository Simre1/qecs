module Main (main) where

import BasicSimulation
import Qecs.Compile.Compile (compile, viewCode)
import Qecs.Store.Map
import Test.Tasty
import Test.Tasty.HUnit


addProgram :: IO (Int -> IO Int)
addProgram = do
  let (computeState, run) = $$(compile addSimulation)
  state <- computeState ""
  pure $ \i -> run state i

queryProgram :: IO (Int -> IO Int)
queryProgram = do
  store1 <- mapStore
  store2 <- mapStore

  let (computeState, run) = $$(compile querySimulation)

  state <- computeState $ World store1 store2
  pure $ \i -> run state i

main :: IO ()
main =
  defaultMain $
    testGroup
      "Simulations"
      [ testCase "Add" $ do
          run <- addProgram
          result <- run 0
          result @?= 30,
        testCase "Query" $ do
          run <- queryProgram
          result <- run 0
          result @?= 100
      ]