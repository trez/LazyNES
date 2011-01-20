module CPU.Cycle
    (
      execute
    ) where

import Prelude hiding (and)

import Helpers (ifM)
import CPU.Types
import CPU.Definition
import CPU.MemoryAddressing
import CPU.Instructions

{-| Executes an instruction.
 -}
execute :: OPCode -> CPU s Int
execute op = case op of
    -- AND
    0x29 -> immediate   >>= and >> return 2
    0x25 -> zeropage    >>= and >> return 3
    0x35 -> zeropageX   >>= and >> return 4
    0x2D -> absolute    >>= and >> return 4
    0x3D -> absoluteX   >>= and >> return 4
    0x39 -> absoluteY   >>= and >> return 4
    0x21 -> indirectX   >>= and >> return 6
    0x31 -> indirectY   >>= and >> return 5
    -- OR
    0x09 -> immediate   >>= ora >> return 2
    0x05 -> zeropage    >>= ora >> return 3
    0x15 -> zeropageX   >>= ora >> return 4
    0x0D -> absolute    >>= ora >> return 4
    0x1D -> absoluteX   >>= ora >> return 4
    0x19 -> absoluteY   >>= ora >> return 4
    0x01 -> indirectX   >>= ora >> return 6
    0x11 -> indirectY   >>= ora >> return 5
    -- EOR
    0x49 -> immediate   >>= eor >> return 2
    0x45 -> zeropage    >>= eor >> return 3
    0x55 -> zeropageX   >>= eor >> return 4
    0x4D -> absolute    >>= eor >> return 4
    0x5D -> absoluteX   >>= eor >> return 4
    0x59 -> absoluteY   >>= eor >> return 4
    0x41 -> indirectX   >>= eor >> return 6
    0x51 -> indirectY   >>= eor >> return 5
    -- BIT
    0x24 -> zeropage    >>= bit >> return 3
    0x2C -> absolute    >>= bit >> return 4
    -- DEC
    0xC6 -> zeropage    >>= dec >> return 5
    0xD6 -> zeropageX   >>= dec >> return 6
    0xCE -> absolute    >>= dec >> return 6
    0xDE -> absoluteX   >>= dec >> return 7
    -- DEX / DEY
    0xCA -> implicit    >>= dex >> return 2
    0x88 -> implicit    >>= dey >> return 2
    -- INC
    0xE6 -> zeropage    >>= inc >> return 5
    0xF6 -> zeropageX   >>= inc >> return 6
    0xEE -> absolute    >>= inc >> return 6
    0xFE -> absoluteX   >>= inc >> return 7
    -- INX / INY
    0xE8 -> implicit    >>= inx >> return 2
    0xC8 -> implicit    >>= iny >> return 2
    -- BCS / BCC
    0xB0 -> relative    >>= bcs >>= \b -> ifM b 3 2
    0x90 -> relative    >>= bcc >>= \b -> ifM b 3 2
    -- BEQ / BNE
    0xF0 -> relative    >>= beq >>= \b -> ifM b 3 2
    0xD0 -> relative    >>= bne >>= \b -> ifM b 3 2
    -- BMI / BPL
    0x30 -> relative    >>= bmi >>= \b -> ifM b 3 2
    0x10 -> relative    >>= bpl >>= \b -> ifM b 3 2
    -- BVC / BVS
    0x50 -> relative    >>= bvc >>= \b -> ifM b 3 2
    0x70 -> relative    >>= bvs >>= \b -> ifM b 3 2
