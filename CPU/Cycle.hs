module CPU.Cycle
    (
      decode
    ) where

import Prelude hiding (and)

import Helpers
import CPU.Types
import CPU.Definition
import CPU.Addressing
import CPU.Instructions

-- | Looks up the correct memory addressing and instruction for a specific
-- opcode and executes it and returns the number of cpu cycles.
decode :: OPCode -> CPU s Int
-- Logical
decode 0x29 = immediate   >>= and
decode 0x25 = zeropage    >>= and
decode 0x35 = zeropageX   >>= and
decode 0x2D = absolute    >>= and
decode 0x3D = absoluteX   >>= and
decode 0x39 = absoluteY   >>= and
decode 0x21 = indirectX   >>= and
decode 0x31 = indirectY   >>= and

decode 0x09 = immediate   >>= ora
decode 0x05 = zeropage    >>= ora
decode 0x15 = zeropageX   >>= ora
decode 0x0D = absolute    >>= ora
decode 0x1D = absoluteX   >>= ora
decode 0x19 = absoluteY   >>= ora
decode 0x01 = indirectX   >>= ora
decode 0x11 = indirectY   >>= ora

decode 0x49 = immediate   >>= eor
decode 0x45 = zeropage    >>= eor
decode 0x55 = zeropageX   >>= eor
decode 0x4D = absolute    >>= eor
decode 0x5D = absoluteX   >>= eor
decode 0x59 = absoluteY   >>= eor
decode 0x41 = indirectX   >>= eor
decode 0x51 = indirectY   >>= eor

decode 0x24 = zeropage    >>= bit
decode 0x2C = absolute    >>= bit

-- Arithmetic.
decode 0x69 = immediate   >>= adc
decode 0x65 = zeropage    >>= adc
decode 0x75 = zeropageX   >>= adc
decode 0x6D = absolute    >>= adc
decode 0x7D = absoluteX   >>= adc
decode 0x79 = absoluteY   >>= adc
decode 0x61 = indirectX   >>= adc
decode 0x71 = indirectY   >>= adc

decode 0xE9 = immediate   >>= sbc
decode 0xE5 = zeropage    >>= sbc
decode 0xF5 = zeropageX   >>= sbc
decode 0xED = absolute    >>= sbc
decode 0xFD = absoluteX   >>= sbc
decode 0xF9 = absoluteY   >>= sbc
decode 0xE1 = indirectX   >>= sbc
decode 0xF1 = indirectY   >>= sbc

decode 0xC9 = immediate   >>= cmp
decode 0xC5 = zeropage    >>= cmp
decode 0xD5 = zeropageX   >>= cmp
decode 0xCD = absolute    >>= cmp
decode 0xDD = absoluteX   >>= cmp
decode 0xD9 = absoluteY   >>= cmp
decode 0xC1 = indirectX   >>= cmp
decode 0xD1 = indirectY   >>= cmp

decode 0xE0 = immediate   >>= cpx
decode 0xE4 = zeropage    >>= cpx
decode 0xEC = absolute    >>= cpx

decode 0xC0 = immediate   >>= cpy
decode 0xC4 = zeropage    >>= cpy
decode 0xCC = absolute    >>= cpy

-- Stack operations.
decode 0xBA = implicit    >>= tsx
decode 0x9A = implicit    >>= txs
decode 0x48 = implicit    >>= pha
decode 0x08 = implicit    >>= php
decode 0x68 = implicit    >>= pla
decode 0x28 = implicit    >>= plp

-- Register transfer.
decode 0x8A = implicit    >>= txa
decode 0x98 = implicit    >>= tya
decode 0xAA = implicit    >>= tax
decode 0xA8 = implicit    >>= tay

-- Load and Store operations.
decode 0xA9 = immediate   >>= lda
decode 0xA5 = zeropage    >>= lda
decode 0xB5 = zeropageX   >>= lda
decode 0xAD = absolute    >>= lda
decode 0xBD = absoluteX   >>= lda
decode 0xB9 = absoluteY   >>= lda
decode 0xA1 = indirectX   >>= lda
decode 0xB1 = indirectY   >>= lda

decode 0xA2 = immediate   >>= ldx
decode 0xA6 = zeropage    >>= ldx
decode 0xB6 = zeropageX   >>= ldx
decode 0xAE = absolute    >>= ldx
decode 0xBE = absoluteY   >>= ldx

decode 0xA0 = immediate   >>= ldy
decode 0xA4 = zeropage    >>= ldy
decode 0xB4 = zeropageX   >>= ldy
decode 0xAC = absolute    >>= ldy
decode 0xBC = absoluteX   >>= ldy

decode 0x85 = zeropage    >>= sta
decode 0x95 = zeropageX   >>= sta
decode 0x8D = absolute    >>= sta
decode 0x9D = absoluteX   >>= sta
decode 0x99 = absoluteY   >>= sta
decode 0x81 = indirectX   >>= sta
decode 0x91 = indirectY   >>= sta

decode 0x86 = zeropage    >>= stx
decode 0x96 = zeropageY   >>= stx
decode 0x8E = absolute    >>= stx

decode 0x84 = zeropage    >>= sty
decode 0x94 = zeropageX   >>= sty
decode 0x8C = absolute    >>= sty

-- Increments and Decrements.
decode 0xC6 = zeropage    >>= dec
decode 0xD6 = zeropageX   >>= dec
decode 0xCE = absolute    >>= dec
decode 0xDE = absoluteX   >>= dec
decode 0xCA = implicit    >>= dex
decode 0x88 = implicit    >>= dey

decode 0xE6 = zeropage    >>= inc
decode 0xF6 = zeropageX   >>= inc
decode 0xEE = absolute    >>= inc
decode 0xFE = absoluteX   >>= inc
decode 0xE8 = implicit    >>= inx
decode 0xC8 = implicit    >>= iny

-- Shifts.
decode 0x0A = accumulator >>= asl
decode 0x06 = zeropage    >>= asl
decode 0x16 = zeropageX   >>= asl
decode 0x0E = absolute    >>= asl
decode 0x1E = absoluteX   >>= asl

decode 0x4A = accumulator >>= lsr
decode 0x46 = zeropage    >>= lsr
decode 0x56 = zeropageX   >>= lsr
decode 0x4E = absolute    >>= lsr
decode 0x5E = absoluteX   >>= lsr

decode 0x6A = accumulator >>= ror
decode 0x66 = zeropage    >>= ror
decode 0x76 = zeropageX   >>= ror
decode 0x6E = absolute    >>= ror
decode 0x7E = absoluteX   >>= ror

decode 0x2A = accumulator >>= rol
decode 0x26 = zeropage    >>= rol
decode 0x36 = zeropageX   >>= rol
decode 0x2E = absolute    >>= rol
decode 0x3E = absoluteX   >>= rol

-- Jumps and calls.
decode 0x20 = absolute    >>= jsr

decode 0x4C = absolute    >>= jmp
decode 0x6C = indirect    >>= jmp

decode 0x60 = implicit    >>= rts

-- Branches.
decode 0xB0 = relative    >>= bcs
decode 0x90 = relative    >>= bcc

decode 0xF0 = relative    >>= beq
decode 0xD0 = relative    >>= bne

decode 0x30 = relative    >>= bmi
decode 0x10 = relative    >>= bpl

decode 0x50 = relative    >>= bvc
decode 0x70 = relative    >>= bvs

-- Status flag changes.
decode 0x38 = implicit    >>= sec
decode 0xF8 = implicit    >>= sed
decode 0x78 = implicit    >>= sei

decode 0x18 = implicit    >>= clc
decode 0xD8 = implicit    >>= cld
decode 0x58 = implicit    >>= cli
decode 0xB8 = implicit    >>= clv

-- System functions.
decode 0x00 = implicit    >>= brk
decode 0x40 = implicit    >>= rti
decode 0xEA = implicit    >>= nop
