module CPU.Instructions
    (
    -- * Logical.
      and
    , ora
    , eor
    , xor
    , asl
    , bit
    -- * Arithmetic.
    -- * Stack operations.
    -- * Register transfer.
    -- * Load and Store operations.
    -- * Increments and Decrements.
    , inc, inx, iny
    , dec, dex, dey

    -- * Shifts.
    -- * Jumps and calls.
    -- * Branches.
    , bcs, bcc -- c
    , beq, bne -- z
    , bmi, bpl -- n
    , bvs, bvc -- v
    -- * Status flag changes.
    -- * System functions.
    ) where

import Prelude hiding (and)

import Control.Monad (when, unless)

import Helpers
import CPU.Types
import CPU.Definition
import CPU.MemoryAddressing

-- ---------------------------------------------------------------------------
-- Logical

{-| Logical AND
 -  A,Z,N = A&M
 -}
and :: Storage -> CPU s ()
and p = fetchValue p >>= alterA . (.&.) >>= setZN

{-| Logical Inclusive OR
 -  A,Z,N = A|M
 -}
ora ::  Storage -> CPU s ()
ora p = fetchValue p >>= alterA . (.|.) >>= setZN

{-| Exclusive OR
 -  A,Z,N = A^M
 -}
eor ::  Storage -> CPU s ()
eor p = fetchValue p >>= alterA . xor >>= setZN

{-| Bit Test
 -  A & M, N = M7, V = M6
 -}
bit :: Storage -> CPU s ()
bit p = do
  val <- fetchValue p
  acc <- getA
  setFlagV $ testBit val 6
  setFlagN $ testBit val 7
  setFlagZ $ (acc .&. val) == 0

-- ---------------------------------------------------------------------------
-- Increments and Decrements.

{-| Decrement Mem
 -  M,Z,N = M-1
 -}
dec :: Storage -> CPU s ()
dec (Memory addr) = alterMemory addr (subtract 1) >>= setZN

{-| Decrement X Reg
 -  X,Z,N = X-1
 -}
dex :: Storage -> CPU s ()
dex Implicit = alterX (subtract 1) >>= setZN

{-| Decrement Y Reg
 -  Y,Z,N = Y-1
 -}
dey :: Storage -> CPU s ()
dey Implicit = alterY (subtract 1) >>= setZN

{-| Increment Mem
 -  M,Z,N = M+1
 -}
inc ::  Storage -> CPU s ()
inc (Memory addr) = alterMemory addr (+1) >>= setZN
-- inc s = alterValue s (+1) >>= setZN

{-| Increment X Reg
 -  X,Z,N = X+1
 -}
inx :: Storage -> CPU s ()
inx Implicit = alterX (+1) >>= setZN

{-| Increment Y Reg
 -  Y,Z,N = Y+1
 -}
iny :: Storage -> CPU s ()
iny Implicit = alterY (+1) >>= setZN

{-| Arithmetic Shift Left
 -  A,Z,C,N = A * 2
 -}
asl :: Storage -> CPU s ()
asl s = do
    setCarryIf s (`testBit` 7)
    val <- alterValue s ((`clearBit` 0) . (`shiftL` 1))
    setZN val

setCarryIf :: Storage -> (Operand -> Bool) -> CPU s ()
setCarryIf s f = fetchValue s >>= setFlagC . f

{-| Branch if carry flag is set
 -}
bcs :: Storage -> CPU s Bool
bcs (Memory addr) = getFlagC >>= \f -> when f (setPC addr) >> return f

{-| Branch if carry flag is clear
 -}
bcc :: Storage -> CPU s Bool
bcc (Memory addr) = getFlagC >>= \f -> unless f (setPC addr) >> return f

{-| Branch if zero flag is set
 -}
beq :: Storage -> CPU s Bool
beq (Memory addr) = getFlagZ >>= \f -> when f (setPC addr) >> return f

{-| Branch if zero flag is clear
 -}
bne :: Storage -> CPU s Bool
bne (Memory addr) = getFlagZ >>= \f -> unless f (setPC addr) >> return f

{-| Branch if negative flag is set
 -}
bmi :: Storage -> CPU s Bool
bmi (Memory addr) = getFlagN >>= \f -> when f (setPC addr) >> return f


{-| Branch if negative flag is clear
 -}
bpl :: Storage -> CPU s Bool
bpl (Memory addr) = getFlagN >>= \f -> unless f (setPC addr) >> return f

{-| Branch if overflow flag is set
 -}
bvs :: Storage -> CPU s Bool
bvs (Memory addr) = getFlagV >>= \f -> when f (setPC addr) >> return f

{-| Branch if overflow flag is clear
 -}
bvc :: Storage -> CPU s Bool
bvc (Memory addr) = getFlagV >>= \f -> unless f (setPC addr) >> return f

