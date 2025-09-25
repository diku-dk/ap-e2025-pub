module Tests (tests) where

import BlockAutomaton.RPS
import BlockAutomaton.Rules
import BlockAutomaton.Simulation
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "All tests"
    []
