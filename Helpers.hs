module Helpers
    (
      -- * Bit
      bitBool
    , boolBit

      -- * Monadic
    , (<+>)
    , whenM
    , unlessM

      -- * Addresses.
    , toAddr
    , mkAddr
    , (<#>)

      -- * Type conversions.
    , signed
    , signed16
    , unsigned
    , unsigned8
    ) where

import Control.Monad (liftM2, when, unless)

import CPU.Types

-- | Set bit @n@ of bits according to @b@.
bitBool :: Bits a => Int -> Bool -> a -> a
bitBool n b = if b then (`setBit` n) else (`clearBit` n)

-- | Return a bits representation of bool.
boolBit :: (Num a, Bits a) => Bool -> a
boolBit True  = 1
boolBit False = 0

-- | Monadic adding.
(<+>) :: (Num a, Monad m) => m a -> m a -> m a
(<+>) = liftM2 (+)

-- | Monadic when.
whenM :: Monad m => m Bool -> m () -> m ()
whenM bm m = do
    b <- bm
    when b m

-- | Monadic unless.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM bm m = do
    b <- bm
    unless b m

-- | Transforms an operand into a Address.
toAddr :: Operand -> Address
toAddr = fromIntegral

-- | Build a 16-bit address from low and high components.
mkAddr :: Operand -> Operand -> Address
mkAddr low high = toAddr high `shiftL` 8 + toAddr low

-- | Builds a 16-bit address.
(<#>) :: Monad m => m Operand -> m Operand -> m Address
(<#>) = liftM2 mkAddr

-- | Transform a unsigned integer to a 8 bit signed integer.
signed :: Word8 -> Int8
signed = fromIntegral

-- | Transform a signed integer to a unsigned 8bit integer.
unsigned :: Int8 -> Word8
unsigned = fromIntegral

-- | Transform a unsigned integer to a 16 bit signed integer without sign promotion.
signed16 :: Word8 -> Int16
signed16 = fromIntegral . (fromIntegral :: Word8 -> Word16)

-- | Transform a 16bit signed integer to a unsigned 8bit integer.
unsigned8 :: Int16 -> Word8
unsigned8 =  fromIntegral . (0x00FF .&.)
