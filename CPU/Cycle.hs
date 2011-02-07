module CPU.Cycle
    (
      execute
    ) where

import Prelude hiding (and)

import Helpers
import CPU.Types
import CPU.Definition
import CPU.MemoryAddressing
import CPU.Instructions

-- | Looks up the correct memory addressing and instruction for a specific 
-- opcode and executes it and returns the number of cpu cycles.
execute :: OPCode -> CPU s Int
execute op = case op of
    -- Logical
    0x29 -> immediate   >>= and >> return 2
    0x25 -> zeropage    >>= and >> return 3
    0x35 -> zeropageX   >>= and >> return 4
    0x2D -> absolute    >>= and >> return 4
    0x3D -> absoluteX   >>= and >> return 4 -- +1
    0x39 -> absoluteY   >>= and >> return 4 -- +1
    0x21 -> indirectX   >>= and >> return 6
    0x31 -> indirectY   >>= and >> return 5 -- +1

    0x09 -> immediate   >>= ora >> return 2
    0x05 -> zeropage    >>= ora >> return 3
    0x15 -> zeropageX   >>= ora >> return 4
    0x0D -> absolute    >>= ora >> return 4
    0x1D -> absoluteX   >>= ora >> return 4 -- +1
    0x19 -> absoluteY   >>= ora >> return 4 -- +1
    0x01 -> indirectX   >>= ora >> return 6
    0x11 -> indirectY   >>= ora >> return 5 -- +1

    0x49 -> immediate   >>= eor >> return 2
    0x45 -> zeropage    >>= eor >> return 3
    0x55 -> zeropageX   >>= eor >> return 4
    0x4D -> absolute    >>= eor >> return 4
    0x5D -> absoluteX   >>= eor >> return 4 -- +1
    0x59 -> absoluteY   >>= eor >> return 4 -- +1
    0x41 -> indirectX   >>= eor >> return 6
    0x51 -> indirectY   >>= eor >> return 5 -- +1

    0x24 -> zeropage    >>= bit >> return 3
    0x2C -> absolute    >>= bit >> return 4

    -- Arithmetic.
    0x69 -> immediate   >>= adc >> return 2
    0x65 -> zeropage    >>= adc >> return 3
    0x75 -> zeropageX   >>= adc >> return 4
    0x6D -> absolute    >>= adc >> return 4
    0x7D -> absoluteX   >>= adc >> return 4 -- +1
    0x79 -> absoluteY   >>= adc >> return 4 -- +1
    0x61 -> indirectX   >>= adc >> return 6
    0x71 -> indirectY   >>= adc >> return 5 -- +1

    0xE9 -> immediate   >>= sbc >> return 2
    0xE5 -> zeropage    >>= sbc >> return 3
    0xF5 -> zeropageX   >>= sbc >> return 4
    0xED -> absolute    >>= sbc >> return 4
    0xFD -> absoluteX   >>= sbc >> return 4 -- +1
    0xF9 -> absoluteY   >>= sbc >> return 4 -- +1
    0xE1 -> indirectX   >>= sbc >> return 6
    0xF1 -> indirectY   >>= sbc >> return 5 -- +1

    0xC9 -> immediate   >>= cmp >> return 2
    0xC5 -> zeropage    >>= cmp >> return 3
    0xD5 -> zeropageX   >>= cmp >> return 4
    0xCD -> absolute    >>= cmp >> return 4
    0xDD -> absoluteX   >>= cmp >> return 4 -- +1
    0xD9 -> absoluteY   >>= cmp >> return 4 -- +1
    0xC1 -> indirectX   >>= cmp >> return 6
    0xD1 -> indirectY   >>= cmp >> return 5 -- +1

    0xE0 -> immediate   >>= cpx >> return 2
    0xE4 -> zeropage    >>= cpx >> return 3
    0xEC -> absolute    >>= cpx >> return 4

    0xC0 -> immediate   >>= cpy >> return 2
    0xC4 -> zeropage    >>= cpy >> return 3
    0xCC -> absolute    >>= cpy >> return 4

    -- Stack operations.
    0xBA -> implicit    >>= tsx >> return 2
    0x9A -> implicit    >>= txs >> return 2
    0x48 -> implicit    >>= pha >> return 3
    0x08 -> implicit    >>= php >> return 3
    0x68 -> implicit    >>= pla >> return 4
    0x28 -> implicit    >>= plp >> return 4

    -- Register transfer.
    0x8A -> implicit    >>= txa >> return 2
    0x98 -> implicit    >>= tya >> return 2
    0xAA -> implicit    >>= tax >> return 2
    0xA8 -> implicit    >>= tay >> return 2
 
    -- Load and Store operations.
    0xA9 -> immediate   >>= lda >> return 2
    0xA5 -> zeropage    >>= lda >> return 3
    0xB5 -> zeropageX   >>= lda >> return 4
    0xAD -> absolute    >>= lda >> return 4
    0xBD -> absoluteX   >>= lda >> return 4 -- +1
    0xB9 -> absoluteX   >>= lda >> return 4 -- +1
    0xA1 -> indirectX   >>= lda >> return 6
    0xB1 -> indirectY   >>= lda >> return 5 -- +1

    0xA2 -> immediate   >>= ldx >> return 2
    0xA6 -> zeropage    >>= ldx >> return 3
    0xB6 -> zeropageX   >>= ldx >> return 4
    0xAE -> absolute    >>= ldx >> return 4
    0xBE -> absoluteY   >>= ldx >> return 4 -- +1

    0xA0 -> immediate   >>= ldy >> return 2
    0xA4 -> zeropage    >>= ldy >> return 3
    0xB4 -> zeropageX   >>= ldy >> return 4
    0xAC -> absolute    >>= ldy >> return 4
    0xBC -> absoluteX   >>= ldy >> return 4 -- +1

    0x85 -> zeropage    >>= sta >> return 3
    0x95 -> zeropageX   >>= sta >> return 4
    0x8D -> absolute    >>= sta >> return 4
    0x9D -> absoluteX   >>= sta >> return 5
    0x99 -> absoluteY   >>= sta >> return 5
    0x81 -> indirectX   >>= sta >> return 6
    0x91 -> indirectY   >>= sta >> return 6

    0x86 -> zeropage    >>= stx >> return 3
    0x96 -> zeropageY   >>= stx >> return 4
    0x8E -> absolute    >>= stx >> return 4

    0x84 -> zeropage    >>= sty >> return 3
    0x94 -> zeropageX   >>= sty >> return 4
    0x8C -> absolute    >>= sty >> return 4

    -- Increments and Decrements.
    0xC6 -> zeropage    >>= dec >> return 5
    0xD6 -> zeropageX   >>= dec >> return 6
    0xCE -> absolute    >>= dec >> return 6
    0xDE -> absoluteX   >>= dec >> return 7
    0xCA -> implicit    >>= dex >> return 2
    0x88 -> implicit    >>= dey >> return 2

    0xE6 -> zeropage    >>= inc >> return 5
    0xF6 -> zeropageX   >>= inc >> return 6
    0xEE -> absolute    >>= inc >> return 6
    0xFE -> absoluteX   >>= inc >> return 7
    0xE8 -> implicit    >>= inx >> return 2
    0xC8 -> implicit    >>= iny >> return 2

    -- Shifts.
    0x0A -> accumulator >>= asl >> return 2
    0x06 -> zeropage    >>= asl >> return 5
    0x16 -> zeropageX   >>= asl >> return 6
    0x0E -> absolute    >>= asl >> return 6
    0x1E -> absoluteX   >>= asl >> return 7

    0x4A -> accumulator >>= lsr >> return 2
    0x46 -> zeropage    >>= lsr >> return 5
    0x56 -> zeropageX   >>= lsr >> return 6
    0x4E -> absolute    >>= lsr >> return 6
    0x5E -> absoluteX   >>= lsr >> return 7

    0x6A -> accumulator >>= ror >> return 2
    0x66 -> zeropage    >>= ror >> return 5
    0x76 -> zeropageX   >>= ror >> return 6
    0x6E -> absolute    >>= ror >> return 6
    0x7E -> absoluteX   >>= ror >> return 7

    0x2A -> accumulator >>= rol >> return 2
    0x26 -> zeropage    >>= rol >> return 5
    0x36 -> zeropageX   >>= rol >> return 6
    0x2E -> absolute    >>= rol >> return 6
    0x3E -> absoluteX   >>= rol >> return 7
 
    -- Jumps and calls.
    0x20 -> absolute    >>= jsr >> return 6

    0x4C -> absolute    >>= jmp >> return 3
    0x6C -> indirect    >>= jmp >> return 5

    0x60 -> implicit    >>= rts >> return 6

    -- Branches.
    0xB0 -> relative    >>= bcs >>= \b -> ifB b 3 2 -- +1, +2
    0x90 -> relative    >>= bcc >>= \b -> ifB b 3 2 -- +1, +2

    0xF0 -> relative    >>= beq >>= \b -> ifB b 3 2 -- +1, +2
    0xD0 -> relative    >>= bne >>= \b -> ifB b 3 2 -- +1, +2

    0x30 -> relative    >>= bmi >>= \b -> ifB b 3 2 -- +1, +2
    0x10 -> relative    >>= bpl >>= \b -> ifB b 3 2 -- +1, +2

    0x50 -> relative    >>= bvc >>= \b -> ifB b 3 2 -- +1, +2
    0x70 -> relative    >>= bvs >>= \b -> ifB b 3 2 -- +1, +2

    -- Status flag changes.
    0x38 -> implicit    >>= sec >> return 2
    0xF8 -> implicit    >>= sed >> return 2
    0x78 -> implicit    >>= sei >> return 2

    0x18 -> implicit    >>= clc >> return 2
    0xD8 -> implicit    >>= cld >> return 2
    0x58 -> implicit    >>= cli >> return 2
    0xB8 -> implicit    >>= clv >> return 2

    -- System functions.
    0x00 -> implicit    >>= brk >> return 7
    0x40 -> implicit    >>= rti >> return 6
    0xEA -> implicit    >>= nop >> return 2
