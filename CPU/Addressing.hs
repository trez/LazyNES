-- |
-- Module:      CPU.Addressing
-- Copyright:   Tobias Vehkajarvi
--
module CPU.Addressing
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
  , Addressing (..)
  , AddressMode (..)
  , Storage (..)
  , fetchValue, fetchAddress
  , storeValue
  , alterValue
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

data AddressMode =
    ACC | IMM | IMP | REL
  | ZPG | ZPX | ZPY
  | ABS | ABX | ABY
  | INX | INY | IND
  deriving (Show,Eq)

data Addressing = Addressing
  { mode      :: AddressMode
  , pageCross :: Bool
  , storage   :: Storage
  }

storeValue :: Addressing -> Operand -> CPU s ()
storeValue Addressing{storage=Accumulator} = setA
storeValue Addressing{storage=Memory addr} = writeMemory addr

fetchValue :: Addressing -> CPU s Operand
fetchValue Addressing{storage=Accumulator} = getA
fetchValue Addressing{storage=Memory addr} = readMemory addr
fetchValue Addressing{storage=Value v}     = return v

alterValue :: Addressing -> (Operand -> Operand) -> CPU s Operand
alterValue Addressing{storage=Accumulator} = alterA
alterValue Addressing{storage=Memory addr} = alterMemory addr

fetchAddress :: Addressing -> CPU s Address
fetchAddress Addressing{storage=Memory addr} = return addr

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
zeropage :: CPU s Addressing
zeropage = do
  addr <- zeropageIndex (return 0x00)
  return Addressing { mode      = ZPG
                    , pageCross = False
                    , storage   = Memory addr }

{-| Indexed zero page addressing adds the value of register x to the value 
 -  of the 8 bit operand to address something in the first page.
 -  Example: STY $10,X ; Store the Y register at location $00(10+X).
 -}
zeropageX :: CPU s Addressing
zeropageX = do
  addr <- zeropageIndex getX
  return Addressing { mode      = ZPX
                    , pageCross = False
                    , storage   = Memory addr }

{-| Indexed zero page addressing adds the value of register y to the value
 - of the 8 bit operand to address somehting in the first page.
 -}
zeropageY :: CPU s Addressing
zeropageY = do
  addr <- zeropageIndex getY
  return Addressing { mode      = ZPY
                    , pageCross = False
                    , storage   = Memory addr }

{-| Add 'idx' to a 8 bit operand.
 -}
zeropageIndex :: CPU s Operand -> CPU s Address
zeropageIndex idx = fromIntegral <$> (idx <+> operand)

{-| Fetch two bytes from stack and construct a absolute address.
 -}
absolute ::  CPU s Addressing
absolute = do
  addr <- address
  return Addressing { mode      = ABS
                    , pageCross = False
                    , storage   = Memory addr }

{-| Fetches absolute address and adds register x.
 -}
absoluteX :: CPU s Addressing
absoluteX = absoluteIndex (Addressing{mode=ABX}) getX

{-| Fetches absolute address and adds register y.
 -}
absoluteY :: CPU s Addressing
absoluteY = absoluteIndex (Addressing{mode=ABY}) getY

{-| Adds the value of the index register to fetched absolute address.
 -}
absoluteIndex :: Addressing -> CPU s Operand -> CPU s Addressing
absoluteIndex a idx = do
  lsb <- fetch
  msb <- fetch
  reg <- idx
  return $ pageCrossing a lsb msb reg

{-| Indirect
 -}
indirect :: CPU s Addressing
indirect = do
  lsbAddr <- absolute >>= fetchAddress
  addr <- readMemory lsbAddr <#> readMemory (wrapPageBoundary lsbAddr)
  return Addressing { mode      = IND
                    , pageCross = False
                    , storage   = Memory $ addr }

{-| Indexed Indirect
 -}
indirectX :: CPU s Addressing
indirectX = do
  lsbAddr <- zeropageX >>= fetchAddress
  addr <- readMemory lsbAddr <#> readMemory (wrapPageBoundary lsbAddr)
  return Addressing { mode      = INX
                    , pageCross = False
                    , storage   = Memory addr }

{-| Indirect Indexed
 -}
indirectY :: CPU s Addressing
indirectY = do
  lsbAddr <- zeropage >>= fetchAddress
  lsb  <- readMemory lsbAddr
  msb  <- readMemory (wrapPageBoundary lsbAddr)
  regY <- getY
  return $ pageCrossing (Addressing{mode=INY}) lsb msb regY

{-| FIXME:
 -}
relative :: CPU s Addressing
relative = do
  pc <- getPC
  op <- (fromIntegral . signed) <$> operand
  let addr = pc + op
  return Addressing { mode      = REL
                    , pageCross = (pc .&. 0xFF00) /= (addr .&. 0xFF00)
                    , storage   = Memory $ addr }

{-| Constant 8 bit value.
 -}
immediate :: CPU s Addressing
immediate = do
  opValue <- operand
  return Addressing { mode      = IMM
                    , pageCross = False
                    , storage   = Value opValue }

{-|
 -}
implicit :: CPU s Addressing
implicit = return Addressing { mode      = IMP
                             , pageCross = False
                             , storage   = Implicit }

{-|
 -}
accumulator :: CPU s Addressing
accumulator = return Addressing { mode      = ACC
                                , pageCross = False
                                , storage   = Accumulator }

-- |
pageCrossing :: Addressing -> Operand -> Operand -> Operand -> Addressing
pageCrossing a lsb msb reg = a { pageCross = reg + lsb < lsb
                               , storage   = Memory $ lsb `mkAddr` msb + toAddr reg }

-- |
wrapPageBoundary :: Address -> Address
wrapPageBoundary addr | (addr .&. 0x00FF) == 0xFF = addr - 0xFF
                      | otherwise                 = addr + 1


