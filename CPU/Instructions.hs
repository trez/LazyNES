module CPU.Instructions
    (
      -- * Load and store operations.
        lda
      , ldx
      , ldy
      , sta
      , sty
      , stx
    ) where

import CPU.Types
import CPU.Definition

-- ===========================================================================
-- = Load and store operations.
-- ===========================================================================
-- | Load memory into accumulator.
lda :: Address -> CPU s ()
lda addr = do
    op <- readMemory addr
    setA op
    setZN op

-- | Load memory into index x.
ldx :: Address -> CPU s ()
ldx addr = do
    op <- readMemory addr
    setX op
    setZN op

-- | Load memory into index y.
ldy :: Address -> CPU s ()
ldy addr = do
    op <- readMemory addr
    setY op
    setZN op

-- | Store accumulator in memory.
sta :: Address -> CPU s ()
sta addr = getA >>= writeMemory addr

-- | Store index register x in memory.
stx :: Address -> CPU s ()
stx addr = getX >>= writeMemory addr

-- | Store index register y in memory.
sty :: Address -> CPU s ()
sty addr = getY >>= writeMemory addr
