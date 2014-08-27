module CPU.Types
    ( module ST
    , module STRef
    , module STArr
    , module I
    , module W
    , module B
    , module CPU.Types
    ) where

import Data.Int  as I
import Data.Word as W
import Data.Bits as B hiding (bit)

--import Control.Monad.ST.Lazy as ST
--import Data.STRef.Lazy as STR

import Control.Monad.ST.Strict as ST
import Data.STRef.Strict       as STRef
import Data.Array.ST           as STArr

type Operand    = W.Word8
type OPCode     = W.Word8
type Address    = W.Word16
type Pixel      = W.Word32 -- ^ RGB Value in hex.
type Status     = W.Word8

type Memory s = STUArray s Address Operand
