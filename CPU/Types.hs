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

import Control.Monad.ST as ST
import Data.STRef       as STRef
import Data.Array.ST    as STArr

type Operand    = Word8
type OPCode     = Word8
type Address    = Word16
type Pixel      = Word32 -- ^ RGB Value in hex.
