-- |
-- Module:      CPU.MemoryAddressing
-- Copyright:   Tobias Vehkajärvi
-- 
module CPU.MemoryAddressing 
  ( -- * Zero page
    zeropage, zeropageX, zeropageY
    -- * Absolute addressing
  , absolute, absoluteX, absoluteY
    -- * Indirect addressing
  , indirect, indirectX, indirectY
  , relative
  , immediate
  , implicit
  , accumulator
    -- * Memory
  , Storage (..)
  , fetchValue, fetchAddress
  , storeValue
  ) where

import CPU.Types
import CPU.Definition
import Helpers

import Control.Applicative ((<$>))

data Storage =
    Implicit
  | Accumulator
  | Value Operand
  | Memory !Address

storeValue :: Storage -> Operand -> CPU s ()
storeValue Accumulator   = setA
storeValue (Memory addr) = writeMemory addr

fetchValue :: Storage -> CPU s Operand
fetchValue Accumulator   = getA
fetchValue (Memory addr) = readMemory addr
fetchValue (Value v)     = return v

fetchAddress (Memory addr) = return addr

{-| Fetch an 8 bit address operand from the stack
 -}
operand :: CPU s Operand
operand = fetch

{-| Fetch two 8 bit address operands from the stack and use them as a low 
 -  and high value of a 16 bit address.
 -}
address :: CPU s Address
address = fetch <#> fetch

{-| Zero page addressing uses only an 8 bit address operand to address the
 -  first 256 bytes in memory (e.g. $0000 to $00FF).
 -  Example: LDA $0F ; Load accumulator from $000F.
 -}
zeropage :: CPU s Storage
zeropage = Memory <$> zeropageIndex (return 0x00)

{-| Indexed zero page addressing adds the value of register x to the value 
 -  of the 8 bit operand to address something in the first page.
 -  Example: STY $10,X ; Store the Y register at location $00(10+X).
 -}
zeropageX :: CPU s Storage
zeropageX = Memory <$> zeropageIndex getX

{-| Indexed zero page addressing ...
 -}
zeropageY :: CPU s Storage
zeropageY = Memory <$> zeropageIndex getY

{-| Add 'idx' to a 8 bit operand.
 -}
zeropageIndex :: CPU s Operand -> CPU s Address
zeropageIndex idx = fromIntegral <$> (idx <+> operand)

{-| Fetch two bytes from stack and construct a absolute address.
 -}
absolute ::  CPU s Storage
absolute = Memory <$> address

{-| Fetches absolute address and adds register x.
 -}
absoluteX :: CPU s Storage
absoluteX = Memory <$> absolutIndex getX

{-| Fetches absolute address and adds register y.
 -}
absoluteY :: CPU s Storage
absoluteY = Memory <$> absolutIndex getY

{-| Adds the value of the index register to fetched absolute address.
 -}
absolutIndex :: CPU s Operand -> CPU s Address
absolutIndex idx = address <+> (fromIntegral <$> idx)

{-| Indirect
 -}
indirect :: CPU s Storage
indirect = Memory <$> (absolute >>= indirectBug)

{-| Indexed Indirect
 -}
indirectX :: CPU s Storage
indirectX = Memory <$> (zeropageX >>= indirectBug)

{-| Indirect Indexed
 -}
indirectY :: CPU s Storage
indirectY = Memory <$> (zeropage >>= indirectBug) <+> (fromIntegral <$> getY)

-- offset bug in 6502, wraps on page boundary. Address wrapping
indirectBug :: Storage -> CPU s Address
indirectBug m = do
    addr <- fetchAddress m
    let addr' | (addr .&. 0x00FF) == 0xFF = addr - 0xFF
              | otherwise                 = addr + 1
    readMemory addr <#> readMemory addr'

{-| FIXME:
 -}
relative :: CPU s Storage
relative = Memory <$> ((fromIntegral . signed) <$> operand) <+> getPC

signed :: Word8 -> Int8
signed = fromIntegral

{-| Constant 8 bit value.
 -}
immediate :: CPU s Storage
immediate = Value <$> operand

{-| 
 -}
implicit :: CPU s Storage
implicit = return Implicit

{-| 
 -}
accumulator :: CPU s Storage
accumulator = return Accumulator
