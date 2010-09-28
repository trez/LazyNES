module Main
    (
      -- * Main
      main
    ) where

import Control.Monad (ap)

import CPU.Types
import CPU.Definition
import CPU.Helpers

main = do
    putStrLn "Running LazyNES, almost :)"
    putStrLn . show . runST $ testProgram

cpuEnv :: ST s (CPUEnv s)
cpuEnv = return CPUEnv
    `ap` newSTRef 0x0                    -- ^ A
    `ap` newSTRef 0x0                    -- ^ X
    `ap` newSTRef 0x0                    -- ^ Y
    `ap` newSTRef 0xFF                   -- ^ SP
    `ap` newSTRef 0x8000                 -- ^ PC
    `ap` newSTRef 0x0                    -- ^ Status
    `ap` newArray (0x0000, 0x07FF) 0x0   -- ^ Lower memory
    `ap` newArray (0x2000, 0x2007) 0x0   -- ^ PPU registers
    `ap` newArray (0x4000, 0xFFFF) 0x0   -- ^ Upper memory

testProgram :: ST s Operand
testProgram = do
    env <- cpuEnv
    runCPU m env
  where 
    m = do
        setA 5
        a <- getA
        return (a+5)
