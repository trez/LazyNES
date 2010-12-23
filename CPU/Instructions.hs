module CPU.Instructions
    ( 
    -- * Logical instructions.
      and
    , ora
    , xor
    , bit
    -- * Arithmetic instructions.
    -- * Stack operations.
    -- * Register transfer.
    -- * Load and Store operations.
    -- * Increments and Decrements.
    -- * Shifts.
    -- * Jumps and calls.
    -- * Branches.
    -- * Status flag changes.
    -- * System functions.
    ) where

import Prelude hiding (and)

import CPU.Types
import CPU.Definition
import CPU.MemoryAddressing

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
bit :: Storage -> CPUST s ()
bit p = do
  val <- fetchValue p
  acc <- getA
  setFlagV $ testBit val 6
  setFlagN $ testBit val 7
  setFlagZ $ (acc .&. val) == 0



