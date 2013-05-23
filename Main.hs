{-# LANGUAGE MultiParamTypeClasses,BangPatterns #-}
module Main
    (
      -- * Main
      main
    ) where

import System.Environment (getArgs)
import Control.Monad
import Debug.Trace

import Helpers
import CPU.Types
import CPU.Definition
import CPU.Addressing
import CPU.Instructions

-- | Main function
main :: IO ()
main = do
    putStrLn "Running LazyNES Test(s)"
    args <- getArgs
    let maxCycles = case args of
                      []    -> 1000000
                      (x:_) -> read x

    putStr "Testing ADC ... "
    putStrLn . show $ runST $ initCPU >>= runCPU adcTest

    putStr "Testing SBC ... "
    putStrLn . show $ runST $ initCPU >>= runCPU sbcTest

    putStr "Testing Branch instructions ... "
    putStrLn . show $ runST $ initCPU >>= runCPU branchTest

    putStr "Running benchmark tests ... "
    putStrLn . show $ runST $ initCPU >>= runCPU (benchTest maxCycles)

--
makeAbsoluteAddress addr = Addressing { mode      = ABS
                                      , pageCross = False
                                      , storage   = Memory addr }
--
makeConstantValue val = Addressing { mode      = IMM
                                   , pageCross = False
                                   , storage   = Value val }

--
fetchExecuteTest = fetch >>= execute

--
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

-- ---------------------------------------------------------------------------
adcTest = do
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
 --    A  + M     (+1)  =  Res  'C'    'V'
    [((88,  70,   True),  (159, False, True))  -- 88  + 70 + 1 = 159
    ,((58,  46,   True),  (105, False, False)) -- 58  + 46 + 1 = 105
    ,((1,   1,    True),  (3,   False, False)) -- 1   + 1  + 1 = 3
    ,((12,  34,   False), (46,  False, False)) -- 12  + 34 + 0 = 46
    ,((15,  26,   False), (41,  False, False)) -- 15  + 26 + 0 = 41
    ,((81,  92,   False), (173, False, True))  -- 81  + 92 + 0 = 173 (overflow)
    ,((127, 1,    False), (128, False, True))  -- 127 + 1  + 0 = 128 (overflow)
    ,((1,   0xFF, False), (0,   True,  False)) -- 1   + (-1) + 0 = 0 (carry)
    ]

-- ---------------------------------------------------------------------------
sbcTest = do
    -- A = A - M - 1 + C
    Prelude.and <$> mapM test testCasesSBC
  where
    test ((a, v, c), (v', nF, cF, oF)) = do
      setFlagC c
      setA a
      sbc $ makeConstantValue v
      b1 <- (v' ==) <$> getA
      b2 <- (nF ==) <$> getFlagN
      b3 <- (cF ==) <$> getFlagC
      b4 <- (oF ==) <$> getFlagV
      return (b1 && b2 && b3 && b4)

testCasesSBC =
 --    A - M - 1 (+'C')  =  Res 'N'    'C'    'V'
    [((0,  0,    False),   (-1, True,  False, False))  --  0 -  0 - 1 + 0 = -1
    ,((0,  0,    True),    (0,  False, True,  False))  --  0 -  0 - 1 + 1 =  0
    -- ,((0,  1,    False),   (0,  False, False, False))  --  0 - -1 - 1 + 0 =  0
    -- ,((0,  1,    True),    (1,  True,  False, True))   --  0 - -1 - 1 + 1 =  1
    -- ,((1,  0,    False),   (-2, False, True,  True))   -- -1 -  0 - 1 + 0 = -2
    -- ,((1,  0,    True),    (-1, True,  True,  False))  -- -1 -  0 - 1 + 1 = -1
    ,((1,  1,    False),   (-1, True,  False, False))  -- -1 -  0 - 1 + 0 = -1
    ,((1,  1,    True),    (0,  False, True,  False))  -- -1 -  0 - 1 + 1 =  0
    ]

-- ---------------------------------------------------------------------------
branchTest = do
    setPC 0x80F0
    pc <- getPC
    writeMemory pc     0xB0 -- BCS
    writeMemory (pc+1) 0x06 -- +6

    -- Execute instruction
    setFlagC False
    cycles1 <- fetchExecuteTest
    pc1 <- getPC

    -- Execute instruction again.
    setPC pc
    setFlagC True
    cycles2 <- fetchExecuteTest
    pc2 <- getPC

    -- Execute instruction again.
    setPC pc
    setFlagC True
    writeMemory (pc+1) 125  -- +125
    cycles3 <- fetchExecuteTest
    pc3 <- getPC

--  trace
--    (  "BCS-False | PC:" ++ show pc ++ "\tPC':" ++ show pc1 ++ "\tCycles:" ++ show cycles1 ++ "\n"
--    ++ "BCS-True  | PC:" ++ show pc ++ "\tPC':" ++ show pc2 ++ "\tCycles:" ++ show cycles2 ++ "\n"
--    ++ "BCS-True  | PC:" ++ show pc ++ "\tPC':" ++ show pc3 ++ "\tCycles:" ++ show cycles3 ++ "\n"
--    )
    return $ (pc+2   == pc1) && (cycles1 == 2)
          && (pc+7   == pc2) && (cycles2 == 3)
          && (pc+126 == pc3) && (cycles3 == 4)

-- ---------------------------------------------------------------------------
benchTest :: Integer -> CPU s Bool
benchTest maxCycles = do
    setPC 0x8000
    setA 10
    setFlagC False
    writeMemory 0x9000 10
    writeMemory 0x8000 0x6D -- ADC ABS
    writeMemory 0x8001 0x00 -- Mem Addr LSB: 00 \
    writeMemory 0x8002 0x90 -- Mem Addr MSB: 90 / 0x9000
    writeMemory 0x8003 0xED -- SBC ABS
    writeMemory 0x8004 0x00 -- Mem Addr LSB: 00 \
    writeMemory 0x8005 0x90 -- Mem Addr MSB: 90 / 0x9000
    writeMemory 0x8006 0x4C -- JMP ABS
    writeMemory 0x8007 0x00 -- Mem Addr LSB: 00 \
    writeMemory 0x8008 0x80 -- Mem Addr MSB: 80 / 0x8000

    instructions <- loopTest 0 maxCycles
    a <- getA
    c <- getFlagC
    v <- getFlagV
    trace
      (  "A:"             ++ show a
      ++ " C:"            ++ show c
      ++ " V:"            ++ show v
      ++ " Instructions:" ++ show instructions
      ++ " Cycles:"       ++ show maxCycles ++ "\n" )
      return $ (a == 20)

loopTest :: Integer -> Integer -> CPU s Integer
loopTest (!i) (!n) | n > 0 = do m <- fromIntegral <$> fetchExecuteTest
                                loopTest (i+1) (n-m)
                   | otherwise = return i
