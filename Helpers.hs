module Helpers
    (
      -- * Bit
      bitBool

      -- * Monadic
    , (<+>)

      -- * Addresses.
    , toAddr
    , mkAddr
    , (<#>)
    ) where

import Control.Monad (liftM2)

import CPU.Types

-- | Set bit @n@ according to @b@.
bitBool :: Bits a => Int -> Bool -> a -> a
bitBool n b = if b then (`setBit` n) else (`clearBit` n)

-- | Monadic adding.
(<+>) :: (Num a, Monad m) => m a -> m a -> m a
(<+>) = liftM2 (+)

-- | Transforms an operand into a Address.
toAddr :: Operand -> Address
toAddr = fromIntegral

-- | Build a 16-bit address from low and high components.
mkAddr :: Operand -> Operand -> Address
mkAddr low high = toAddr high `shiftL` 8 + toAddr low

-- | Builds a 16-bit address.
(<#>) :: Monad m => m Operand -> m Operand -> m Address
(<#>) = liftM2 mkAddr
