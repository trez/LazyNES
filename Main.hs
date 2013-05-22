module Main
    (
      -- * Main
      main
    ) where

import Control.Monad
import Debug.Trace

import Helpers
import CPU.Types
import CPU.Definition
import CPU.Addressing
import CPU.Instructions
import CPU.Cycle

-- | Main function
main :: IO ()
main = do
    putStrLn "Running LazyNES Test(s)"
    let i = runST $ do
        env <- initCPU
        runCPU m env
    putStr "Testing ADC ... "
    putStrLn (show i)
  where
    m = do
      implicit >>= inx
      implicit >>= inx
      setFlagC True
      writeMemory 0x2000 1
      adc $ makeAbsoluteAddress 0x2000
      getA
      testADC

makeAbsoluteAddress addr = Addressing { mode      = ABS
                                      , pageCross = False
                                      , storage   = Memory addr }

makeConstantValue val = Addressing { mode      = IMM
                                   , pageCross = False
                                   , storage   = Value val }

initCPU :: ST s (CPUEnv s)
initCPU = do
    return CPUEnv
        `ap` newSTRef 0x0                    -- A
        `ap` newSTRef 0x0                    -- X
        `ap` newSTRef 0x0                    -- Y
        `ap` newSTRef 0xFF                   -- SP
        `ap` newSTRef 0x8000                 -- PC
        `ap` newSTRef 0x0                    -- Status
        `ap` newArray (0x0000, 0x07FF) 0x0   -- Lower memory
        `ap` newArray (0x2000, 0x2007) 0x0   -- PPU registers
        `ap` newArray (0x4000, 0xFFFF) 0x0   -- Upper memory

testADC = do
    Prelude.and <$> mapM test testCasesADC
  where
    test ((a, v, c), (v', cF, oF)) = do
      setFlagC c
      setA a
      adc $ makeConstantValue v
      b1 <- (v' ==) <$> getA
      b2 <- (cF ==) <$> getFlagC
      b3 <- (oF ==) <$> getFlagV
      return (b1 && b2 && b3)

testCasesADC =
 --    X  + Y     (+1)  =  Res  'C'    'V'
    [((88,  70,   True),  (159, False, True))  -- 88  + 70 + 1 = 159
    ,((58,  46,   True),  (105, False, False)) -- 58  + 46 + 1 = 105
    ,((1,   1,    True),  (3,   False, False)) -- 1   + 1  + 1 = 3
    ,((12,  34,   False), (46,  False, False)) -- 12  + 34 + 0 = 46
    ,((15,  26,   False), (41,  False, False)) -- 15  + 26 + 0 = 41
    ,((81,  92,   False), (173, False, True))  -- 81  + 92 + 0 = 173 (overflow)
    ,((127, 1,    False), (128, False, True))  -- 127 + 1  + 0 = 128 (overflow)
    ,((1,   0xFF, False), (0,   True,  False)) -- 1   + (-1) + 0 = 0 (carry)
    ]
