module CPU.Instructions
    (
      execute

    -- * Logical.
    , and
    , ora
    , eor
    , bit

    -- * Arithmetic.
    , adc, sbc
    , cmp
    , cpx
    , cpy

    -- * Stack operations.
    , tsx, txs
    , pha, pla
    , php, plp

    -- * Register transfer.
    , tax, txa
    , tay, tya

    -- * Load and Store operations.
    , lda, sta
    , ldx, stx
    , ldy, sty

    -- * Increments and Decrements.
    , inc, dec
    , inx, dex
    , iny, dey

    -- * Shifts.
    , asl, lsr
    , rol, ror

    -- * Jumps and calls.
    , jsr
    , jmp
    , rts

    -- * Branches.
    , bcs, bcc -- c
    , beq, bne -- z
    , bmi, bpl -- n
    , bvs, bvc -- v

    -- * Status flag changes.
    , sec, clc -- c
    , sed, cld -- d
    , sei, cli -- i
    , clv      -- v

    -- * System functions.
    , brk
    , nop
    , rti
    ) where

import Prelude hiding (and)

import qualified Data.Bits as Bits
import Control.Monad
import Debug.Trace

import Helpers (unlessM, whenM, signed16, unsigned8, boolBit, bitBool, (<#>))
import CPU.Types
import CPU.Definition
import CPU.Addressing

checkExtraCycle :: Addressing -> Int -> Int
checkExtraCycle Addressing{pageCross = True} c = c + 1
checkExtraCycle _PageCrossFalse              c = c

-- | Looks up the correct memory addressing and instruction for a specific
-- opcode and executes it and returns the number of cpu cycles.
execute :: OPCode -> CPU s Int
-- Logical
execute 0x29 = immediate   >>= and
execute 0x25 = zeropage    >>= and
execute 0x35 = zeropageX   >>= and
execute 0x2D = absolute    >>= and
execute 0x3D = absoluteX   >>= and
execute 0x39 = absoluteY   >>= and
execute 0x21 = indirectX   >>= and
execute 0x31 = indirectY   >>= and

execute 0x09 = immediate   >>= ora
execute 0x05 = zeropage    >>= ora
execute 0x15 = zeropageX   >>= ora
execute 0x0D = absolute    >>= ora
execute 0x1D = absoluteX   >>= ora
execute 0x19 = absoluteY   >>= ora
execute 0x01 = indirectX   >>= ora
execute 0x11 = indirectY   >>= ora

execute 0x49 = immediate   >>= eor
execute 0x45 = zeropage    >>= eor
execute 0x55 = zeropageX   >>= eor
execute 0x4D = absolute    >>= eor
execute 0x5D = absoluteX   >>= eor
execute 0x59 = absoluteY   >>= eor
execute 0x41 = indirectX   >>= eor
execute 0x51 = indirectY   >>= eor

execute 0x24 = zeropage    >>= bit
execute 0x2C = absolute    >>= bit

-- Arithmetic.
execute 0x69 = immediate   >>= adc
execute 0x65 = zeropage    >>= adc
execute 0x75 = zeropageX   >>= adc
execute 0x6D = absolute    >>= adc
execute 0x7D = absoluteX   >>= adc
execute 0x79 = absoluteY   >>= adc
execute 0x61 = indirectX   >>= adc
execute 0x71 = indirectY   >>= adc

execute 0xE9 = immediate   >>= sbc
execute 0xE5 = zeropage    >>= sbc
execute 0xF5 = zeropageX   >>= sbc
execute 0xED = absolute    >>= sbc
execute 0xFD = absoluteX   >>= sbc
execute 0xF9 = absoluteY   >>= sbc
execute 0xE1 = indirectX   >>= sbc
execute 0xF1 = indirectY   >>= sbc

execute 0xC9 = immediate   >>= cmp
execute 0xC5 = zeropage    >>= cmp
execute 0xD5 = zeropageX   >>= cmp
execute 0xCD = absolute    >>= cmp
execute 0xDD = absoluteX   >>= cmp
execute 0xD9 = absoluteY   >>= cmp
execute 0xC1 = indirectX   >>= cmp
execute 0xD1 = indirectY   >>= cmp

execute 0xE0 = immediate   >>= cpx
execute 0xE4 = zeropage    >>= cpx
execute 0xEC = absolute    >>= cpx

execute 0xC0 = immediate   >>= cpy
execute 0xC4 = zeropage    >>= cpy
execute 0xCC = absolute    >>= cpy

-- Stack operations.
execute 0xBA = implicit    >>= tsx
execute 0x9A = implicit    >>= txs
execute 0x48 = implicit    >>= pha
execute 0x08 = implicit    >>= php
execute 0x68 = implicit    >>= pla
execute 0x28 = implicit    >>= plp

-- Register transfer.
execute 0x8A = implicit    >>= txa
execute 0x98 = implicit    >>= tya
execute 0xAA = implicit    >>= tax
execute 0xA8 = implicit    >>= tay

-- Load and Store operations.
execute 0xA9 = immediate   >>= lda
execute 0xA5 = zeropage    >>= lda
execute 0xB5 = zeropageX   >>= lda
execute 0xAD = absolute    >>= lda
execute 0xBD = absoluteX   >>= lda
execute 0xB9 = absoluteY   >>= lda
execute 0xA1 = indirectX   >>= lda
execute 0xB1 = indirectY   >>= lda

execute 0xA2 = immediate   >>= ldx
execute 0xA6 = zeropage    >>= ldx
execute 0xB6 = zeropageX   >>= ldx
execute 0xAE = absolute    >>= ldx
execute 0xBE = absoluteY   >>= ldx

execute 0xA0 = immediate   >>= ldy
execute 0xA4 = zeropage    >>= ldy
execute 0xB4 = zeropageX   >>= ldy
execute 0xAC = absolute    >>= ldy
execute 0xBC = absoluteX   >>= ldy

execute 0x85 = zeropage    >>= sta
execute 0x95 = zeropageX   >>= sta
execute 0x8D = absolute    >>= sta
execute 0x9D = absoluteX   >>= sta
execute 0x99 = absoluteY   >>= sta
execute 0x81 = indirectX   >>= sta
execute 0x91 = indirectY   >>= sta

execute 0x86 = zeropage    >>= stx
execute 0x96 = zeropageY   >>= stx
execute 0x8E = absolute    >>= stx

execute 0x84 = zeropage    >>= sty
execute 0x94 = zeropageX   >>= sty
execute 0x8C = absolute    >>= sty

-- Increments and Decrements.
execute 0xC6 = zeropage    >>= dec
execute 0xD6 = zeropageX   >>= dec
execute 0xCE = absolute    >>= dec
execute 0xDE = absoluteX   >>= dec
execute 0xCA = implicit    >>= dex
execute 0x88 = implicit    >>= dey

execute 0xE6 = zeropage    >>= inc
execute 0xF6 = zeropageX   >>= inc
execute 0xEE = absolute    >>= inc
execute 0xFE = absoluteX   >>= inc
execute 0xE8 = implicit    >>= inx
execute 0xC8 = implicit    >>= iny

-- Shifts.
execute 0x0A = accumulator >>= asl
execute 0x06 = zeropage    >>= asl
execute 0x16 = zeropageX   >>= asl
execute 0x0E = absolute    >>= asl
execute 0x1E = absoluteX   >>= asl

execute 0x4A = accumulator >>= lsr
execute 0x46 = zeropage    >>= lsr
execute 0x56 = zeropageX   >>= lsr
execute 0x4E = absolute    >>= lsr
execute 0x5E = absoluteX   >>= lsr

execute 0x6A = accumulator >>= ror
execute 0x66 = zeropage    >>= ror
execute 0x76 = zeropageX   >>= ror
execute 0x6E = absolute    >>= ror
execute 0x7E = absoluteX   >>= ror

execute 0x2A = accumulator >>= rol
execute 0x26 = zeropage    >>= rol
execute 0x36 = zeropageX   >>= rol
execute 0x2E = absolute    >>= rol
execute 0x3E = absoluteX   >>= rol

-- Jumps and calls.
execute 0x20 = absolute    >>= jsr

execute 0x4C = absolute    >>= jmp
execute 0x6C = indirect    >>= jmp

execute 0x60 = implicit    >>= rts

-- Branches.
execute 0xB0 = relative    >>= bcs
execute 0x90 = relative    >>= bcc

execute 0xF0 = relative    >>= beq
execute 0xD0 = relative    >>= bne

execute 0x30 = relative    >>= bmi
execute 0x10 = relative    >>= bpl

execute 0x50 = relative    >>= bvc
execute 0x70 = relative    >>= bvs

-- Status flag changes.
execute 0x38 = implicit    >>= sec
execute 0xF8 = implicit    >>= sed
execute 0x78 = implicit    >>= sei

execute 0x18 = implicit    >>= clc
execute 0xD8 = implicit    >>= cld
execute 0x58 = implicit    >>= cli
execute 0xB8 = implicit    >>= clv

-- System functions.
execute 0x00 = implicit    >>= brk
execute 0x40 = implicit    >>= rti
execute 0xEA = implicit    >>= nop

{-----------------------------------------------------------------------------
  * Logical
-----------------------------------------------------------------------------}
-- | Logical AND
-- A,Z,N = A&M
and :: Addressing -> CPU s Int
and p = do
  fetchValue p >>= alterA . (.&.) >>= alterStatus . setZN
  return . cycles . mode $ p
  where cycles IMM = 2
        cycles ZPG = 3
        cycles ZPX = 4
        cycles ABS = 4
        cycles ABX = checkExtraCycle p 4
        cycles ABY = checkExtraCycle p 4
        cycles INX = 6
        cycles INY = checkExtraCycle p 5

-- | Logical Inclusive OR
-- A,Z,N = A|M
ora ::  Addressing -> CPU s Int
ora p = do fetchValue p >>= alterA . (.|.) >>= alterStatus . setZN
           return . cycles . mode $ p
  where cycles IMM = 2
        cycles ZPG = 3
        cycles ZPX = 4
        cycles ABS = 4
        cycles ABX = checkExtraCycle p 4
        cycles ABY = checkExtraCycle p 4
        cycles INX = 6
        cycles INY = checkExtraCycle p 5

-- | Exclusive OR
-- A,Z,N = A^M
eor ::  Addressing -> CPU s Int
eor p = do fetchValue p >>= alterA . xor >>= alterStatus . setZN
           return . cycles . mode $ p
  where cycles IMM = 2
        cycles ZPG = 3
        cycles ZPX = 4
        cycles ABS = 4
        cycles ABX = checkExtraCycle p 4
        cycles ABY = checkExtraCycle p 4
        cycles INX = 6
        cycles INY = checkExtraCycle p 5

-- | Bit Test
-- A & M, N = M7, V = M6
bit :: Addressing -> CPU s Int
bit p = do
  val <- fetchValue p
  acc <- getA
  alterStatus $ setFlagV (testBit val 6)
              . setFlagN (testBit val 7)
              . setFlagZ ((acc .&. val) == 0)
  return . cycles . mode $ p
  where cycles ZPG = 3
        cycles ABS = 4

{-----------------------------------------------------------------------------
  * Increments and Decrements.
-----------------------------------------------------------------------------}
-- | Decrement Mem
-- M,Z,N = M-1
dec :: Addressing -> CPU s Int
dec a = do
  addr <- fetchAddress a
  alterMemory addr (subtract 1) >>= alterStatus . setZN
  return . cycles . mode $ a
  where cycles ZPG = 5
        cycles ZPX = 6
        cycles ABS = 6
        cycles ABX = 7

-- | Decrement X Reg
-- X,Z,N = X-1
dex :: Addressing -> CPU s Int
dex Addressing{storage=Implicit} = do alterX (subtract 1) >>= alterStatus . setZN
                                      return 2

-- | Decrement Y Reg
-- Y,Z,N = Y-1
dey :: Addressing -> CPU s Int
dey Addressing{storage=Implicit} = do alterY (subtract 1) >>= alterStatus . setZN
                                      return 2

-- | Increment Mem
-- M,Z,N = M+1
inc :: Addressing -> CPU s Int
inc a = do
  addr <- fetchAddress a
  alterMemory addr (+1) >>= alterStatus . setZN
  return . cycles . mode $ a
  where cycles ZPG = 5
        cycles ZPX = 6
        cycles ABS = 6
        cycles ABX = 7

-- | Increment X Reg
-- X,Z,N = X+1
inx :: Addressing -> CPU s Int
inx Addressing{storage=Implicit} = do alterX (+1) >>= alterStatus . setZN
                                      return 2

-- | Increment Y Reg
-- Y,Z,N = Y+1
iny :: Addressing -> CPU s Int
iny Addressing{storage=Implicit} = do alterY (+1) >>= alterStatus . setZN
                                      return 2

{-----------------------------------------------------------------------------
  * Arithmetic.
-----------------------------------------------------------------------------}
-- | Addition with carry
-- A,Z,C,N = A+M+C
adc :: Addressing -> CPU s Int
adc p = do
  v <- signed16 <$> fetchValue p
  a <- signed16 <$> getA
  c <- boolBit  <$> getFlagC  -- Representing 0 / 1
  let temp = a + v + c
  a' <- alterA . const . fromIntegral $ temp
  alterStatus $ setZN    a'
              . setFlagC (temp `testBit` 8)
              . setFlagV (((complement $ a `xor` v) .&.  (a `xor` temp))
                          `testBit` 7)
  return . cycles . mode $ p
  where cycles IMM = 2
        cycles ZPG = 3
        cycles ZPX = 4
        cycles ABS = 4
        cycles ABX = checkExtraCycle p 4
        cycles ABY = checkExtraCycle p 4
        cycles INX = 6
        cycles INY = checkExtraCycle p 5

-- | Subtraction with carry
-- A,Z,C,N = A-M-(1-C)
sbc :: Addressing -> CPU s Int
sbc p = do
    v <- fetchValue p
    adc p{storage=Value $ v `xor` 0xFF}

-- | Compare
-- Z,C,N = A-M
cmp :: Addressing -> CPU s Int
cmp s = do compares (fetchValue s) getA
           return . cycles . mode $ s
  where cycles IMM = 2
        cycles ZPG = 3
        cycles ZPX = 4
        cycles ABS = 4
        cycles ABX = checkExtraCycle s 4
        cycles ABY = checkExtraCycle s 4
        cycles INX = 6
        cycles INY = checkExtraCycle s 5

-- | Compare X Reg
-- Z,C,N = X-M
cpx :: Addressing -> CPU s Int
cpx s = do compares (fetchValue s) getX
           return . cycles . mode $ s
  where cycles IMM = 2
        cycles ZPG = 3
        cycles ABS = 4

-- | Compare Y Reg
-- Z,C,N = Y-M
cpy :: Addressing -> CPU s Int
cpy s = do compares (fetchValue s) getY
           return . cycles . mode $ s
  where cycles IMM = 2
        cycles ZPG = 3
        cycles ABS = 4

-- | Helper function that does the comparsion.
compares :: CPU s Operand -> CPU s Operand -> CPU s ()
compares mem reg = do
    a <- mem
    b <- reg
    alterStatus $ setFlagC (b >= a)
                . setFlagZ (a == b)
                . setFlagN ((b - a) < 0)

{-----------------------------------------------------------------------------
  * Shifts.
-----------------------------------------------------------------------------}
-- | Arithmetic Shift Left
--  A,Z,C,N = A * 2
asl :: Addressing -> CPU s Int
asl s = do
    v  <- fetchValue s
    v' <- alterValue s $ (`clearBit` 0) . (`shiftL` 1)
    alterStatus $ setZN  v'
                . setFlagC (v `testBit` 7)
    return . cycles . mode $ s
  where cycles ACC = 2
        cycles ZPG = 5
        cycles ZPX = 6
        cycles ABS = 6
        cycles ABX = 7

-- | Logical Shift Right
-- A,Z,C,N = A * 2
lsr :: Addressing -> CPU s Int
lsr s = do
    v  <- fetchValue s
    v' <- alterValue s $ (`clearBit` 7) . (`shiftR` 1)
    alterStatus $ setZN v'
                . setFlagC (v `testBit` 0)
    return . cycles . mode $ s
  where cycles ACC = 2
        cycles ZPG = 5
        cycles ZPX = 6
        cycles ABS = 6
        cycles ABX = 7

-- | Rotate Left
-- Z,N,C,M
rol :: Addressing -> CPU s Int
rol s =  do
    v  <- fetchValue s
    c  <- getFlagC
    v' <- alterValue s $ (bitBool 0 c) . (`shiftL` 1)
    alterStatus $ setZN v'
                . setFlagC (v `testBit` 7)
    return . cycles . mode $ s
  where cycles ACC = 2
        cycles ZPG = 5
        cycles ZPX = 6
        cycles ABS = 6
        cycles ABX = 7

-- | Rotate Right
-- Z,N,C,M
ror :: Addressing -> CPU s Int
ror s =  do
    v  <- fetchValue s
    c  <- getFlagC
    v' <- alterValue s $ (bitBool 7 c) . (`shiftR` 1)
    alterStatus $ setZN v'
                . setFlagC (v `testBit` 0)
    return . cycles . mode $ s
  where cycles ACC = 2
        cycles ZPG = 5
        cycles ZPX = 6
        cycles ABS = 6
        cycles ABX = 7

{-----------------------------------------------------------------------------
  * Branches.
 ----------------------------------------------------------------------------}
-- | Branch if carry flag is set
bcs :: Addressing -> CPU s Int
bcs = branchOn getFlagC

-- | Branch if carry flag is clear
bcc :: Addressing -> CPU s Int
bcc = branchOn (not <$> getFlagC)

-- | Branch if zero flag is set
beq :: Addressing -> CPU s Int
beq = branchOn getFlagZ

-- | Branch if zero flag is clear
bne :: Addressing -> CPU s Int
bne = branchOn (not <$> getFlagZ)

-- | Branch if negative flag is set
bmi :: Addressing -> CPU s Int
bmi = branchOn getFlagN

-- | Branch if negative flag is clear
bpl :: Addressing -> CPU s Int
bpl = branchOn (not <$> getFlagN)

-- | Branch if overflow flag is set
bvs :: Addressing -> CPU s Int
bvs = branchOn getFlagV

-- | Branch if overflow flag is clear
bvc :: Addressing -> CPU s Int
bvc = branchOn (not <$> getFlagV)

branchOn :: CPU s Bool -> Addressing -> CPU s Int
branchOn flagM a = do
  b <- flagM
  case b of
    True  -> do addr <- fetchAddress a
                setPC addr
                return $ checkExtraCycle a 3
    False -> return 2

{-----------------------------------------------------------------------------
  * System functions.
-----------------------------------------------------------------------------}
-- | Set the break flag, force interrupt
-- B = 1, I = 1
brk :: Addressing -> CPU s Int
brk Addressing{storage=Implicit} = do
    alterPC (+1)
    pushPC
    alterStatus $ setFlagB True
    getStatus >>= push
    alterStatus $ setFlagI True
    readMemory 0xFFFE <#> readMemory 0xFFFF >>= setPC
    return 7

-- | No operation.
nop :: Addressing -> CPU s Int
nop Addressing{storage=Implicit} = return 2

-- | Return from Interrupt
-- C,Z,I,D,B,V,N,PC
rti :: Addressing -> CPU s Int
rti Addressing{storage=Implicit} = do
    pull   >>= setStatus
    pullPC >>= setPC
    return 6

{-----------------------------------------------------------------------------
  * Status flag changes.
-----------------------------------------------------------------------------}
-- | Clear carry flag
-- C = 0
clc :: Addressing -> CPU s Int
clc Addressing{storage=Implicit} = alterStatus (setFlagC False) >> return 2

-- | Clear decimal mode flag
-- D = 0
cld :: Addressing -> CPU s Int
cld Addressing{storage=Implicit} = alterStatus (setFlagD False) >> return 2

-- | Clear interrupt disable flag
-- I = 0
cli :: Addressing -> CPU s Int
cli Addressing{storage=Implicit} = alterStatus (setFlagI False) >> return 2

-- | Clear overflow flag
-- V = 0
clv :: Addressing -> CPU s Int
clv Addressing{storage=Implicit} = alterStatus (setFlagV False) >> return 2

-- | Set carry flag
-- C = 1
sec :: Addressing -> CPU s Int
sec Addressing{storage=Implicit} = alterStatus (setFlagC True) >> return 2

-- | Set decimal mode flag
-- D = 1
sed :: Addressing -> CPU s Int
sed Addressing{storage=Implicit} = alterStatus (setFlagD True) >> return 2

-- | Set interrupt disable flag
-- I = 1
sei :: Addressing -> CPU s Int
sei Addressing{storage=Implicit} = alterStatus (setFlagI True) >> return 2

{-----------------------------------------------------------------------------
  * Load and Store operations.
-----------------------------------------------------------------------------}
-- | Load memory into accumulator
-- A,Z,N = M
lda :: Addressing -> CPU s Int
lda s = do fetchValue s >>= alterA . const >>= alterStatus . setZN
           return . cycles . mode $ s
  where cycles IMM = 2
        cycles ZPG = 3
        cycles ZPX = 4
        cycles ABS = 4
        cycles ABX = checkExtraCycle s 4
        cycles ABY = checkExtraCycle s 4
        cycles INX = 6
        cycles INY = checkExtraCycle s 5

-- | Load memory into index x
-- X,Z,N = M
ldx :: Addressing -> CPU s Int
ldx s = do fetchValue s >>= alterX . const >>= alterStatus . setZN
           return . cycles . mode $ s
  where cycles IMM = 2
        cycles ZPG = 3
        cycles ZPY = 4
        cycles ABS = 4
        cycles ABY = checkExtraCycle s 4

-- | Load memory into index y
-- Y,Z,N = M
ldy :: Addressing -> CPU s Int
ldy s = do fetchValue s >>= alterY . const >>= alterStatus . setZN
           return . cycles . mode $ s
  where cycles IMM = 2
        cycles ZPG = 3
        cycles ZPX = 4
        cycles ABS = 4
        cycles ABX = checkExtraCycle s 4

-- | Store accumulator in memory
-- M = A
sta :: Addressing -> CPU s Int
sta s = do st' getA s
           return . cycles . mode $ s
  where cycles ZPG = 3
        cycles ZPX = 4
        cycles ABS = 4
        cycles ABX = 5
        cycles ABY = 5
        cycles INX = 6
        cycles INY = 6

-- | Store index register x in memory
-- M = X
stx :: Addressing -> CPU s Int
stx s = do st' getX s
           return . cycles . mode $ s
  where cycles ZPG = 3
        cycles ZPY = 4
        cycles ABS = 4

-- | Store index register y in memory
-- M = Y
sty :: Addressing -> CPU s Int
sty s = do st' getY s
           return . cycles . mode $ s
  where cycles ZPG = 3
        cycles ZPX = 4
        cycles ABS = 4


st' :: CPU s Operand -> Addressing -> CPU s ()
st' val a = do
  addr <- fetchAddress a
  val >>= writeMemory addr

-- | Transfer A to X
-- Z,N,X = A
tax :: Addressing -> CPU s Int
tax Addressing{storage=Implicit} = do
  getA >>= alterX . const >>= alterStatus . setZN
  return 2

-- | Transfer A to Y
-- Z,N,Y = A
tay :: Addressing -> CPU s Int
tay Addressing{storage=Implicit} = do
  getA >>= alterY . const >>= alterStatus . setZN
  return 2

-- | Transfer X to A
-- Z,N,A = X
txa :: Addressing -> CPU s Int
txa Addressing{storage=Implicit} = do
  getX >>= alterA . const >>= alterStatus . setZN
  return 2

-- | Transfer Y to A
-- Z,N,A = Y
tya :: Addressing -> CPU s Int
tya Addressing{storage=Implicit} = do
  getY >>= alterA . const >>= alterStatus . setZN
  return 2

{-----------------------------------------------------------------------------
  * Stack operations.
-----------------------------------------------------------------------------}
-- | Transfer X to SP
-- SP = X
txs :: Addressing -> CPU s Int
txs Addressing{storage=Implicit} = do
  regX <- getX >>= alterSP . const . fromIntegral
  alterStatus . setZN . fromIntegral $ regX
  return 2

-- | Transfer SP to X
-- Z,N,X = SP
tsx :: Addressing -> CPU s Int
tsx Addressing{storage=Implicit} = do
  getSP >>= alterX . const . fromIntegral >>= alterStatus . setZN
  return 2

-- | Push a to stack
-- M(SP) = A
pha :: Addressing -> CPU s Int
pha Addressing{storage=Implicit} = do
  getA >>= push
  return 3

-- | Push status to stack
-- M(SP) = P
php :: Addressing -> CPU s Int
php Addressing{storage=Implicit} = do
  alterStatus (setFlagB True) >> getStatus >>= push
  return 3

-- | Pull a from stack
-- A,Z,N = M(SP)
pla :: Addressing -> CPU s Int
pla Addressing{storage=Implicit} = do
  pull >>= \val -> setA val >> alterStatus (setZN val)
  return 4

-- | Pull status from stack
-- P = M(SP)
plp :: Addressing -> CPU s Int
plp Addressing{storage=Implicit} = do
  pull >>= setStatus
  return 4

{-----------------------------------------------------------------------------
  * Jumps and calls.
-----------------------------------------------------------------------------}
-- | Jump to subrutine
jsr :: Addressing -> CPU s Int
jsr s = alterPC (subtract 1) >> pushPC >> jmp s >> return 6

-- | Jump to address
jmp :: Addressing -> CPU s Int
jmp a = do fetchAddress a >>= setPC
           return . cycles . mode $ a
  where cycles ABS = 3
        cycles IND = 5

-- | Return to subrutine
rts :: Addressing -> CPU s Int
rts Addressing{storage=Implicit} = pullPC >>= setPC . (+1) >> return 6
