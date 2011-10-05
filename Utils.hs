{-# LANGUAGE NoImplicitPrelude #-}

module Utils ( unsafeFromForeignPtr0, unsafeToForeignPtr0 ) where

------------------------------------------------------------------------
-- Imports
------------------------------------------------------------------------

-- from base:
import Data.Int           ( Int )
import Foreign.Storable   ( Storable )
import Foreign.ForeignPtr ( ForeignPtr )

-- from vector:
import Data.Vector.Storable ( Vector, unsafeFromForeignPtr, unsafeToForeignPtr )

------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------

unsafeFromForeignPtr0 :: Storable a
                      => ForeignPtr a
                      -> Int
                      -> Vector a
unsafeFromForeignPtr0 fp n = unsafeFromForeignPtr fp 0 n
{-# INLINE unsafeFromForeignPtr0 #-}

unsafeToForeignPtr0 :: Storable a => Vector a -> (ForeignPtr a, Int)
unsafeToForeignPtr0 v = let (fp, _, n) = unsafeToForeignPtr v
                        in (fp, n)
{-# INLINE unsafeToForeignPtr0 #-}
