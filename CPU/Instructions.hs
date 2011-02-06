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

import Control.Monad (when, unless)
import Debug.Trace

import Helpers
import CPU.Types
import CPU.Definition
import CPU.MemoryAddressing

-- ---------------------------------------------------------------------------
-- * Logical

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
-- * Increments and Decrements.
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

-- ---------------------------------------------------------------------------
-- * Arithmetic.
{-| Addition with carry
 -  A,Z,C,N = A+M+C
 -}
adc :: Storage -> CPU s ()
adc s = do
    v <- signed16 <$> fetchValue s
    a <- signed16 <$> getA
    c <- boolBit  <$> getFlagC
    let temp = a + v + c
    a' <- alterA . const . unsigned8 $ temp
    setZN    $ a'
    setFlagV $ ((complement $ a `xor` v) .&. (a `xor` temp)) `testBit` 7
    setFlagC $ temp `testBit` 8

{-| Subtraction with carry
 -  A,Z,C,N = A-M-(1-C)
 -}
sbc :: Storage -> CPU s ()
sbc s = do
    v <- fetchValue s
    adc (Value $ v `xor` 0xFF)

cmp :: Storage -> CPU s ()
cmp s = compares (fetchValue s) getA

cpx :: Storage -> CPU s ()
cpx s = compares (fetchValue s) getX

cpy :: Storage -> CPU s ()
cpy s = compares (fetchValue s) getY

compares :: CPU s Operand -> CPU s Operand -> CPU s ()
compares mem reg = do
    a <- mem
    b <- reg
    setFlagC $ b >= a
    setFlagZ $ a == b
    setFlagN $ (b - a) < 0

-- ---------------------------------------------------------------------------
-- * Shifts.
{-| Arithmetic Shift Left
 -  A,Z,C,N = A * 2
 -}
asl :: Storage -> CPU s ()
asl s = do
    v  <- fetchValue s
    v' <- alterValue s $ (`clearBit` 0) . (`shiftL` 1)
    setZN    $ v'
    setFlagC $ v `testBit` 7

{-| Logical Shift Right
 -  A,Z,C,N = A * 2
 -}
lsr :: Storage -> CPU s ()
lsr s = do
    v  <- fetchValue s
    v' <- alterValue s $ (`clearBit` 7) . (`shiftR` 1)
    setZN    $ v'
    setFlagC $ v `testBit` 0

{-| Rotate Left
 -  Z,N,C,M
 -}
rol :: Storage -> CPU s ()
rol s =  do
    v  <- fetchValue s
    c  <- getFlagC
    v' <- alterValue s $ (bitBool 0 c) . (`shiftL` 1)
    setZN    $ v'
    setFlagC $ v `testBit` 7

{-| Rotate Right
 -  Z,N,C,M
 -}
ror :: Storage -> CPU s ()
ror s =  do
    v  <- fetchValue s
    c  <- getFlagC
    v' <- alterValue s $ (bitBool 7 c) . (`shiftR` 1)
    setZN    $ v'
    setFlagC $ v `testBit` 0

-- ---------------------------------------------------------------------------
-- * Branches.
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

-- ---------------------------------------------------------------------------
-- * System functions.
{-| Set the break flag, force interrupt
 -  B = 1
 -}
brk :: Storage -> CPU s ()
brk Implicit = do
    alterPC succ
    pushPC
    setFlagB True
    getStatus >>= push
    setFlagI True
    readMemory 0xFFFE <#> readMemory 0xFFFF >>= setPC

{- FIME: documentation
 -}
nop :: Storage -> CPU s ()
nop Implicit = return ()

{-| Return from Interrupt
 -  C,Z,I,D,B,V,N,PC
 -}
rti :: Storage -> CPU s ()
rti Implicit = do
    pull   >>= setStatus
    pullPC >>= setPC


-- ---------------------------------------------------------------------------
-- * Status flag changes.
{-| Clear carry flag
 -  C = 0
 -}
clc :: Storage -> CPU s ()
clc Implicit = setFlagC False

{-| Clear decimal mode flag
 -  D = 0
 -}
cld :: Storage -> CPU s ()
cld Implicit = setFlagD False

{-| Clear interrupt disable flag
 -  I = 0
 -}
cli :: Storage -> CPU s ()
cli Implicit = setFlagI False

{-| Clear overflow flag
 -  V = 0
 -}
clv :: Storage -> CPU s ()
clv Implicit = setFlagV False

{-| Set carry flag
 -  C = 1
 -}
sec :: Storage -> CPU s ()
sec Implicit = setFlagC True

{-| Set decimal mode flag
 -  D = 1
 -}
sed :: Storage -> CPU s ()
sed Implicit = setFlagD True

{-| Set interrupt disable flag
 -  I = 1
 -}
sei :: Storage -> CPU s ()
sei Implicit = setFlagI True

-- ---------------------------------------------------------------------------
-- * Load and Store operations.
{-| Load memory into accumulator
 -  A,Z,N = M
 -}
lda :: Storage -> CPU s ()
lda s = fetchValue s >>= alterA . const >>= setZN

{-| Load memory into index x
 -  X,Z,N = M
 -}
ldx :: Storage -> CPU s ()
ldx s = fetchValue s >>= alterX . const >>= setZN

{-| Load memory into index y
 -  Y,Z,N = M
 -}
ldy :: Storage -> CPU s ()
ldy s = fetchValue s >>= alterY . const >>= setZN

{-| Store accumulator in memory
 -  M = A
 -}
sta :: Storage -> CPU s ()
sta (Memory addr) = getA >>= writeMemory addr

{-| Store index register x in memory
 -  M = X
 -}
stx :: Storage -> CPU s ()
stx (Memory addr) = getX >>= writeMemory addr

{-| Store index register y in memory
 -  M = Y
 -}
sty :: Storage -> CPU s ()
sty (Memory addr) = getY >>= writeMemory addr

{-| Transfer A to X
 -  Z,N,X = A
 -}
tax :: Storage -> CPU s ()
tax Implicit = getA >>= alterX . const >>= setZN

{-| Transfer A to Y
 -  Z,N,Y = A
 -}
tay :: Storage -> CPU s ()
tay Implicit = getA >>= alterY . const >>= setZN

{-| Transfer X to A
 -  Z,N,A = X
 -}
txa :: Storage -> CPU s ()
txa Implicit = getX >>= alterA . const >>= setZN

{-| Transfer Y to A
 -  Z,N,A = Y
 -}
tya :: Storage -> CPU s ()
tya Implicit = getY >>= alterA . const >>= setZN

-- ---------------------------------------------------------------------------
-- * Stack operations.
{-| Transfer X to SP
 -  SP = X
 -}
txs :: Storage -> CPU s ()
txs Implicit = getX >>= alterSP . const . fromIntegral >>= setZN . fromIntegral

{-| Transfer SP to X
 -  Z,N,X = SP
 -}
tsx :: Storage -> CPU s ()
tsx Implicit = getSP >>= alterX . const . fromIntegral >>= setZN

{-| Push a to stack
 -  M(SP) = A
 -}
pha :: Storage -> CPU s ()
pha Implicit = getA >>= push

{-| Push status to stack
 -  M(SP) = P
 -}
php :: Storage -> CPU s ()
php Implicit = setFlagB True >> getStatus >>= push

{-| Pull a from stack
 -  A,Z,N = M(SP)
 -}
pla :: Storage -> CPU s ()
pla Implicit = pull >>= \val -> setA val >> setZN val

{-| Pull status from stack
 -  P = M(SP)
 -}
plp :: Storage -> CPU s ()
plp Implicit = pull >>= setStatus

-- ---------------------------------------------------------------------------
-- * Jumps and calls.
{-| Jump to subrutine
 -}
jsr :: Storage -> CPU s ()
jsr s = alterPC (subtract 1) >> pushPC >> jmp s

{-| Jump to address
 -}
jmp :: Storage -> CPU s ()
jmp (Memory addr) = setPC addr

{-| Return to subrutine
 -}
rts :: Storage -> CPU s ()
rts Implicit = pullPC >>= setPC . (+1)
