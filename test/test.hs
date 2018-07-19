import Test.Tasty
import Test.Tasty.HUnit

import qualified CPU.Instructions.Tests

main = defaultMain $ testGroup "Tests"
  [ CPU.Instructions.Tests.tests
  ]
