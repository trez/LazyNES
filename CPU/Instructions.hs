module CPU.Instructions
    ( lAnd
    )where

import CPU.Types
import CPU.Definition
import CPU.MemoryAddressing

{-| Logical AND
 -  A,Z,N = A&M
 -}
lAnd :: Storage -> CPU s ()
lAnd p = fetchValue p >>= alterA . (.&.) >>= setZN
