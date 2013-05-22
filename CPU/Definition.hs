{-# LANGUAGE MultiParamTypeClasses #-}

-- | The CPU definitions.
module CPU.Definition
    (
      -- * The CPU Monad.
      CPU
    , runCPU
    , CPUEnv (..)
    , (<$>)

      -- * Accumulator A
    , getA , setA , alterA

      -- * Register X
    , getX , setX , alterX

      -- * Register Y
    , getY , setY , alterY

      -- * Stack
    , getSP , setSP , alterSP
    , push
    , pull

      -- * Program counter
    , getPC , setPC , alterPC
    , pushPC
    , pullPC

      -- * Status
    , getStatus , setStatus , alterStatus

      -- * Memory
    , readMemory , writeMemory , alterMemory
    , fetch

      -- * Managing flags.
    , getFlagC, getFlagZ, getFlagI, getFlagD, getFlagB
    , getFlagQ, getFlagV, getFlagN
    , setFlagC, setFlagZ, setFlagI, setFlagD, setFlagB
    , setFlagQ, setFlagV, setFlagN
    , setZN
    ) where

import Control.Monad.Reader
import Control.Applicative
import Control.Monad (Functor)

import Helpers (toAddr, bitBool, (<#>))
import CPU.Types

-- | The CPU monad.
newtype CPU s a = CPU { runCPU :: CPUEnv s -> ST s a }

instance Monad (CPU s) where
    return x = CPU $ \_ -> return x
    m >>= f  = CPU $ \r -> do
        a <- runCPU m r
        runCPU (f a) r

instance MonadReader (CPUEnv s) (CPU s) where
    ask       = CPU $ return
    local f m = CPU $ runCPU m . f

instance Functor (CPU s) where
    fmap f m = m >>= return . f

instance Applicative (CPU s) where
    pure = return
    (<*>) = ap

-- | The Environment storing the STRefs.
data CPUEnv s = CPUEnv
    {
      aReg    :: STRef s Operand -- ^ Accumulator, 8 bit
    , xReg    :: STRef s Operand -- ^ X register, 8 bit
    , yReg    :: STRef s Operand -- ^ Y register, 8 bit
    , sp      :: STRef s Operand -- ^ Stack pointer, 8 bit
    , pc      :: STRef s Address -- ^ Program counter, 16 bit
    , status  :: STRef s Operand -- ^ Status register, 8 bit
    , lowMem  :: Memory s        -- ^ Memory range 0000 - 07FF
    , ppuMem  :: Memory s        -- ^ Memory range 2000 - 2007
    , uppMem  :: Memory s        -- ^ Memory range 4000 - FFFF
    }

-- ---------------------------------------------------------------------------
-- Functions for dealing with the environment.

writeArr  :: (CPUEnv s -> Memory s) -> Address -> Operand -> CPU s ()
readArr   :: (CPUEnv s -> Memory s) -> Address -> CPU s Operand
setVar    :: (CPUEnv s -> STRef s a) -> a -> CPU s ()
getVar    :: (CPUEnv s -> STRef s a) -> CPU s a
alterVar  :: (CPUEnv s -> STRef s a) -> (a -> a) -> CPU s a
alterVar_ :: (CPUEnv s -> STRef s a) -> (a -> a) -> CPU s ()

writeArr f a o = CPU $ \r -> writeArray (f r) a o
readArr  f a   = CPU $ \r -> readArray (f r) a
setVar   f a   = CPU $ \r -> writeSTRef (f r) a
getVar   f     = CPU $ \r -> readSTRef (f r)
alterVar f g   = CPU $ \r -> let r' = f r in modifySTRef r' g >> readSTRef r'
alterVar_ f g  = CPU $ \r -> modifySTRef (f r) g

{-----------------------------------------------------------------------------
  * Accumulator (8 bit).
-----------------------------------------------------------------------------}
-- | Get the value of accumulator A from the environment.
getA :: CPU s Operand
getA = getVar aReg

-- | Set accumulator A with a given value.
setA :: Operand -> CPU s ()
setA = setVar aReg

-- | Alter the value of accumulator A.
alterA :: (Operand -> Operand) -> CPU s Operand
alterA = alterVar aReg

{-----------------------------------------------------------------------------
  * X Register (8 bit).
-----------------------------------------------------------------------------}
-- | Get a 8 bit value from the X register.
getX :: CPU s Operand
getX = getVar xReg

-- | Set a 8 bit value to the X register.
setX :: Operand -> CPU s ()
setX = setVar xReg

-- | Alter the value of the X register.
alterX :: (Operand -> Operand) -> CPU s Operand
alterX = alterVar xReg

{-----------------------------------------------------------------------------
  * Y Register.
-----------------------------------------------------------------------------}
-- | Get a 8 bit value from the Y register.
getY :: CPU s Operand
getY = getVar yReg

-- | Set a 8 bit value to the Y register.
setY :: Operand -> CPU s ()
setY = setVar yReg

-- | Alter the value of the Y register.
alterY :: (Operand -> Operand) -> CPU s Operand
alterY = alterVar yReg

{-----------------------------------------------------------------------------
  * Program counter.
-----------------------------------------------------------------------------}
-- | Get the 16 bit program counter value.
getPC :: CPU s Address
getPC = getVar pc

-- | Set a 16 bit value to the program counter.
setPC :: Address -> CPU s ()
setPC = setVar pc

-- | Alter the program counter.
alterPC :: (Address -> Address) -> CPU s Address
alterPC = alterVar pc

-- | Push the program counter onto the stack.
pushPC :: CPU s ()
pushPC = do 
    res <- getPC
    push $ fromIntegral $ res `shiftR` 8 -- high 8 bits
    push $ fromIntegral $ res            -- low 8 bits

-- | Pull the program counter from stack and returns the address.
pullPC :: CPU s Address
pullPC = pull <#> pull

{-----------------------------------------------------------------------------
  * Stack.
-----------------------------------------------------------------------------}
-- | The stack starting position.
baseSP :: Address
baseSP = 0x0100

-- | Get the stack pointer value.
getSP :: CPU s Word8
getSP = getVar sp

-- | Set a 8 bit value to the stack pointer.
setSP :: Word8 -> CPU s ()
setSP = setVar sp

-- | Alter the stack pointer.
alterSP :: (Word8 -> Word8) -> CPU s Word8
alterSP = alterVar sp

-- | Push a operand on the stack.
push :: Operand -> CPU s ()
push op = do
    stackValue <- getSP
    setSP $ stackValue - 1
    writeMemory (baseSP + toAddr stackValue) op

-- | Pull a operand from the stack.
pull :: CPU s Operand
pull = do
    stackValue <- toAddr <$> alterSP (+1) -- Make address
    readMemory $ baseSP + stackValue

{-----------------------------------------------------------------------------
  * Status register including flags.
-----------------------------------------------------------------------------}
-- |
getStatus :: CPU s Operand
getStatus = getVar status

-- |
setStatus :: Operand -> CPU s ()
setStatus = setVar status

-- |
alterStatus :: (Operand -> Operand) -> CPU s ()
alterStatus = alterVar_ status

-- | Get bit `x` of status.
getFlagBit :: Int -> CPU s Bool
getFlagBit x = (`testBit` x) <$> getStatus

-- | Set bit `x` of status according to bool.
setFlagBit :: Int -> Bool -> CPU s ()
setFlagBit x = alterStatus . bitBool x

getFlagC, getFlagZ, getFlagI, getFlagD, getFlagB, getFlagQ, getFlagV
    , getFlagN :: CPU s Bool
getFlagC = getFlagBit 0
getFlagZ = getFlagBit 1
getFlagI = getFlagBit 2
getFlagD = getFlagBit 3
getFlagB = getFlagBit 4
getFlagQ = getFlagBit 5
getFlagV = getFlagBit 6
getFlagN = getFlagBit 7

setFlagC, setFlagZ, setFlagI, setFlagD, setFlagB, setFlagQ, setFlagV
    , setFlagN :: Bool -> CPU s ()
setFlagC = setFlagBit 0
setFlagZ = setFlagBit 1
setFlagI = setFlagBit 2
setFlagD = setFlagBit 3
setFlagB = setFlagBit 4
setFlagQ = setFlagBit 5
setFlagV = setFlagBit 6
setFlagN = setFlagBit 7

-- | Set Z if operand is zero and set N flag if operand has signed bit set.
-- > do setZN 0
-- >    (True  ==) <$> getFlagZ
-- >    (False ==) <$> getFlagN
setZN :: Operand -> CPU s ()
setZN oper = do
    setFlagZ $ oper == 0
    setFlagN $ oper `testBit` 7 -- check if last bit is set.

{-----------------------------------------------------------------------------
  * Memory.
-----------------------------------------------------------------------------}
-- |
readMemory  :: Address -> CPU s Operand
readMemory addr
    | addr < 0x2000 = readArr lowMem (addr `mod` 0x800)
    | addr < 0x4000 = let addr' = 0x2000 + (addr `mod` 8)
                      in readArr ppuMem addr'  -- FIXME: Notify PPU

    | addr == 0x4016 = undefined
    | addr == 0x4017 = undefined
    | otherwise = readArr uppMem addr

-- |
writeMemory :: Address -> Operand -> CPU s ()
writeMemory addr op
    | addr < 0x2000 = writeArr lowMem (addr `mod` 0x800) op
    | addr < 0x4000 =
        let addr' = (0x2000 + addr `mod` 8)
        in writeArr ppuMem addr' op -- FIXME: Notify PPU
    | addr == 0x4014 = do
        addr' <- return op <#> readArr ppuMem 0x2003
        ops   <- forM [addr' .. (addr' + 255)] readMemory
        -- FIXME: PPU DMA
        writeArr uppMem 0x4014 op
    | otherwise = writeArr uppMem addr op

-- |
alterMemory :: Address -> (Operand -> Operand) -> CPU s Operand
alterMemory addr f = do
    res <- f <$> readMemory addr
    writeMemory addr res
    return res

-- | Returns the byte that PC points to in memory and increments PC with one.
fetch :: CPU s OPCode
fetch = do
    op <- getPC
    setPC (op + 1)
    readMemory op
