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

    putStr "Testing Branch instructions ... "
    putStrLn . show $ runST $ initCPU >>= runCPU branchTest

    putStr "Running benchmark tests ... "
    putStrLn . show $ runST $ initCPU >>= runCPU (benchTest maxCycles)

--
makeAbsoluteAddress addr = Addressing { mode      = ABS
                                      , pageCross = False
                                      , storage   = Memory addr }
--
fetchExecuteTest = fetch >>= execute

-- ---------------------------------------------------------------------------
branchTest = do
    setPC 0x80F0
    pc <- getPC
    writeMemory pc     0xB0 -- BCS
    writeMemory (pc+1) 0x06 -- +6

    -- Execute instruction
    alterStatus $ setFlagC False
    cycles1 <- fetchExecuteTest
    pc1 <- getPC

    -- Execute instruction again.
    setPC pc
    alterStatus $ setFlagC True
    cycles2 <- fetchExecuteTest
    pc2 <- getPC

    -- Execute instruction again.
    setPC pc
    alterStatus $ setFlagC True
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
    alterStatus $ setFlagC False
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
    return $ trace
      (  "A:"             ++ show a
      ++ " C:"            ++ show c
      ++ " V:"            ++ show v
      ++ " Instructions:" ++ show instructions
      ++ " Cycles:"       ++ show maxCycles ++ "\n"
      )
     (a == 20)

loopTest :: Integer -> Integer -> CPU s Integer
loopTest (!i) (!n) | n > 0 = do m <- fromIntegral <$> fetchExecuteTest
                                loopTest (i+1) (n-m)
                   | otherwise = return i
