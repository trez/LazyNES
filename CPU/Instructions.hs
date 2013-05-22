module CPU.Instructions
    (
    -- * Logical.
      and
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

import Debug.Trace

import Helpers (unlessM, whenM, signed16, unsigned8, boolBit, bitBool, (<#>))
import CPU.Types
import CPU.Definition
import CPU.Addressing

checkExtraCycle :: Addressing -> Int -> Int
checkExtraCycle Addressing{pageCross = True} c = c + 1
checkExtraCycle _PageCrossFalse              c = c

{-----------------------------------------------------------------------------
  * Logical
-----------------------------------------------------------------------------}
-- | Logical AND
-- A,Z,N = A&M
and :: Addressing -> CPU s Int
and p = do
  val   <- fetchValue p
  valA' <- alterA (.&. val)
  setZN valA'
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
ora p = do fetchValue p >>= alterA . (.|.) >>= setZN
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
eor p = do fetchValue p >>= alterA . xor >>= setZN
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
  setFlagV $ testBit val 6
  setFlagN $ testBit val 7
  setFlagZ $ (acc .&. val) == 0
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
  alterMemory addr (subtract 1) >>= setZN
  return . cycles . mode $ a
  where cycles ZPG = 5
        cycles ZPX = 6
        cycles ABS = 6
        cycles ABX = 7

-- | Decrement X Reg
-- X,Z,N = X-1
dex :: Addressing -> CPU s Int
dex a@Addressing{storage=Implicit} = do
  alterX (subtract 1) >>= setZN
  return . cycles . mode $ a
  where cycles IMM = 2

-- | Decrement Y Reg
-- Y,Z,N = Y-1
dey :: Addressing -> CPU s Int
dey a@Addressing{storage=Implicit} = do
  alterY (subtract 1) >>= setZN
  return . cycles . mode $ a
  where cycles IMM = 2

-- | Increment Mem
-- M,Z,N = M+1
inc :: Addressing -> CPU s Int
inc a = do
  addr <- fetchAddress a
  alterMemory addr (+1) >>= setZN
  return . cycles . mode $ a
  where cycles ZPG = 5
        cycles ZPX = 6
        cycles ABS = 6
        cycles ABX = 7

-- | Increment X Reg
-- X,Z,N = X+1
inx :: Addressing -> CPU s Int
inx Addressing{storage=Implicit} = do
  alterX (+1) >>= setZN
  return 2

-- | Increment Y Reg
-- Y,Z,N = Y+1
iny :: Addressing -> CPU s Int
iny Addressing{storage=Implicit} = do
  alterY (+1) >>= setZN
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
  c <- boolBit  <$> getFlagC
  let temp = a + v + c
  a' <- alterA . const . unsigned8 $ temp
  setZN    $ a'
  setFlagV $ ((complement $ a `xor` v) .&. (a `xor` temp)) `testBit` 7
  setFlagC $ temp `testBit` 8
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
sbc s = do
  v <- fetchValue s
  adc s{storage=Value $ v `xor` 0xFF}

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

cpx :: Addressing -> CPU s Int
cpx s = do compares (fetchValue s) getX
           return . cycles . mode $ s
  where cycles IMM = 2
        cycles ZPG = 3
        cycles ABS = 4

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
    setFlagC $ b >= a
    setFlagZ $ a == b
    setFlagN $ (b - a) < 0

{-----------------------------------------------------------------------------
  * Shifts.
-----------------------------------------------------------------------------}
-- | Arithmetic Shift Left
--  A,Z,C,N = A * 2
asl :: Addressing -> CPU s Int
asl s = do
    v  <- fetchValue s
    v' <- alterValue s $ (`clearBit` 0) . (`shiftL` 1)
    setZN    $ v'
    setFlagC $ v `testBit` 7
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
    setZN    $ v'
    setFlagC $ v `testBit` 0
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
    setZN    $ v'
    setFlagC $ v `testBit` 7
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
    setZN    $ v'
    setFlagC $ v `testBit` 0
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
-- B = 1
brk :: Addressing -> CPU s Int
brk Addressing{storage=Implicit} = do
    alterPC succ
    pushPC
    setFlagB True
    getStatus >>= push
    setFlagI True
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
clc Addressing{storage=Implicit} = setFlagC False >> return 2

-- | Clear decimal mode flag
-- D = 0
cld :: Addressing -> CPU s Int
cld Addressing{storage=Implicit} = setFlagD False >> return 2

-- | Clear interrupt disable flag
-- I = 0
cli :: Addressing -> CPU s Int
cli Addressing{storage=Implicit} = setFlagI False >> return 2

-- | Clear overflow flag
-- V = 0
clv :: Addressing -> CPU s Int
clv Addressing{storage=Implicit} = setFlagV False >> return 2

-- | Set carry flag
-- C = 1
sec :: Addressing -> CPU s Int
sec Addressing{storage=Implicit} = setFlagC True >> return 2

-- | Set decimal mode flag
-- D = 1
sed :: Addressing -> CPU s Int
sed Addressing{storage=Implicit} = setFlagD True >> return 2

-- | Set interrupt disable flag
-- I = 1
sei :: Addressing -> CPU s Int
sei Addressing{storage=Implicit} = setFlagI True >> return 2

{-----------------------------------------------------------------------------
  * Load and Store operations.
-----------------------------------------------------------------------------}
-- | Load memory into accumulator
-- A,Z,N = M
lda :: Addressing -> CPU s Int
lda s = do fetchValue s >>= alterA . const >>= setZN
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
ldx s = do fetchValue s >>= alterX . const >>= setZN
           return . cycles . mode $ s
  where cycles IMM = 2
        cycles ZPG = 3
        cycles ZPY = 4
        cycles ABS = 4
        cycles ABY = checkExtraCycle s 4

-- | Load memory into index y
-- Y,Z,N = M
ldy :: Addressing -> CPU s Int
ldy s = do fetchValue s >>= alterY . const >>= setZN
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
  getA >>= alterX . const >>= setZN
  return 2

-- | Transfer A to Y
-- Z,N,Y = A
tay :: Addressing -> CPU s Int
tay Addressing{storage=Implicit} = do
  getA >>= alterY . const >>= setZN
  return 2

-- | Transfer X to A
-- Z,N,A = X
txa :: Addressing -> CPU s Int
txa Addressing{storage=Implicit} = do
  getX >>= alterA . const >>= setZN
  return 2

-- | Transfer Y to A
-- Z,N,A = Y
tya :: Addressing -> CPU s Int
tya Addressing{storage=Implicit} = do
  getY >>= alterA . const >>= setZN
  return 2

{-----------------------------------------------------------------------------
  * Stack operations.
-----------------------------------------------------------------------------}
-- | Transfer X to SP
-- SP = X
txs :: Addressing -> CPU s Int
txs Addressing{storage=Implicit} = do
  getX >>= alterSP . const . fromIntegral >>= setZN . fromIntegral
  return 2

-- | Transfer SP to X
-- Z,N,X = SP
tsx :: Addressing -> CPU s Int
tsx Addressing{storage=Implicit} = do
  getSP >>= alterX . const . fromIntegral >>= setZN
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
  setFlagB True >> getStatus >>= push
  return 3

-- | Pull a from stack
-- A,Z,N = M(SP)
pla :: Addressing -> CPU s Int
pla Addressing{storage=Implicit} = do
  pull >>= \val -> setA val >> setZN val
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
