module Main (main) where

import Control.Monad
import Data.SparseSet.Storable qualified as S
import Qecs.Compile.Compile (compile, viewCode)
import Qecs.Store.Map
import Qecs.Store.SparseSet
import Test.Tasty
import Test.Tasty.HUnit
import TestSimulations

addProgram :: IO (Int -> IO Int)
addProgram = do
  let (computeState, run) = $$(compile world addSimulation)
  state <- computeState
  pure $ \i -> run state i

queryProgram :: IO (Int -> IO Int)
queryProgram = do
  let (computeState, run) = $$(compile world querySimulation)

  state <- computeState
  pure $ \i -> run state i

resourceProgram :: IO (() -> IO Int)
resourceProgram = do
  let (computeState, run) = $$(compile world resourceSimulation)

  state <- computeState
  pure $ \i -> run state i

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Tests"
      [ecsTests, sparseSetTests]

ecsTests :: TestTree
ecsTests =
  testGroup
    "ECS"
    [ testCase "Add" $ do
        run <- addProgram
        result <- run 0
        result @?= 30,
      testCase "Query" $ do
        run <- queryProgram
        result <- run 0
        result @?= 110,
      testCase "Resources" $ do
        run <- resourceProgram
        result <- run ()
        result @?= 22
    ]

sparseSetTests :: TestTree
sparseSetTests =
  testGroup
    "SparseSet"
    [ testGroup
        "Storable"
        [ testCase "Insert/Lookup/Remove" $ do
            set <- S.create @Int 5 5
            S.size set >>= (@?= 0)
            S.insert set 0 10
            S.insert set 1 11
            S.insert set 2 12
            S.lookup set 0 >>= (@?= Just 10)
            S.lookup set 1 >>= (@?= Just 11)
            S.lookup set 2 >>= (@?= Just 12)
            S.lookup set 3 >>= (@?= Nothing)
            S.unsafeContains set 1 >>= (@?= True)
            S.size set >>= (@?= 3)
            S.remove set 1
            S.size set >>= (@?= 2)
            S.lookup set 1 >>= (@?= Nothing)
            S.lookup set 0 >>= (@?= Just 10)
            S.lookup set 2 >>= (@?= Just 12)
            S.remove set 0
            S.remove set 2
            S.size set >>= (@?= 0)
            S.unsafeContains set 0 >>= (@?= False)
            S.unsafeContains set 2 >>= (@?= False)
            pure ()
        ]
    ]
