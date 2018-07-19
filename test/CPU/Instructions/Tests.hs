module CPU.Instructions.Tests
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad (liftM4)
import Control.Monad.ST.Strict as ST

import CPU.Definition
import CPU.Instructions
import CPU.Addressing
import CPU.Types

tests = testGroup "Tests" [adcTests, sbcTests]

data InputValues = InputValues { aValue :: Operand, constantVal :: Operand, carryFlag :: Bool }
data ExpectedOutput = ExpectedOutput { res :: Operand, flagC :: Bool, flagV :: Bool, flagN :: Bool }

data ArithOP = ADC | SBC deriving Show

adcTests = testGroup "ADC tests" $ testArith ADC $
  [ ((InputValues 88   70   True),  (ExpectedOutput 159  False True  True))
  , ((InputValues 58   46   True),  (ExpectedOutput 105  False False False))
  , ((InputValues 1    1    True),  (ExpectedOutput 3    False False False))
  , ((InputValues 12   34   False), (ExpectedOutput 46   False False False))
  , ((InputValues 15   26   False), (ExpectedOutput 41   False False False))
  , ((InputValues 81   92   False), (ExpectedOutput 173  False True  True))
  , ((InputValues 127  1    False), (ExpectedOutput 128  False True  True))
  , ((InputValues 1    0xFF False), (ExpectedOutput 0    True  False False))
  ]

sbcTests = testGroup "SBC tests" $ testArith SBC $
  [ ((InputValues 0    0    False), (ExpectedOutput (-1) False False True))
  , ((InputValues 0    0    True),  (ExpectedOutput 0    True  False False))
  , ((InputValues 0    (-1) False), (ExpectedOutput 0    False False False))
  , ((InputValues 0    (-1) True),  (ExpectedOutput 1    False False False))
  , ((InputValues (-1) 0    False), (ExpectedOutput (-2) True  False True))
  , ((InputValues (-1) 0    True),  (ExpectedOutput (-1) True  False True))
  , ((InputValues (-1) 1    False), (ExpectedOutput (-3) True  False True))
  , ((InputValues (-1) 1    True),  (ExpectedOutput (-2) True  False True))
  ]

testArith :: ArithOP -> [(InputValues, ExpectedOutput)] -> [TestTree]
testArith op = map (\(i, (inData, outData)) -> testCase (show op ++ " #" ++ show i) $ arithTest op inData outData )
             . zip [1..]

arithTest :: ArithOP -> InputValues -> ExpectedOutput -> Assertion
arithTest op inp expected = do
    let (resA, resFlagC, resFlagV, resFlagN) = ST.runST $ initCPU >>= runCPU test
    assertBool ("Result does not match, got " ++ show resA ++ " expected " ++ show (res expected) )(resA == res expected)
    assertBool "C flag does not match" (resFlagC == flagC expected)
    assertBool "V flag does not match" (resFlagV == flagV expected)
    assertBool "N flag does not match" (resFlagN == flagN expected)
  where
    test = do
      alterStatus $ setFlagC (carryFlag inp)
      setA (aValue inp)
      case op of
        ADC -> adc $ makeConstantValue (constantVal inp)
        SBC -> sbc $ makeConstantValue (constantVal inp)
      liftM4 (,,,) getA getFlagC getFlagV getFlagN

makeConstantValue val = Addressing { mode      = IMM
                                   , pageCross = False
                                   , storage   = Value val }
