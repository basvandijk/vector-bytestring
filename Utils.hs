{-# LANGUAGE CPP, NoImplicitPrelude #-}

module Utils ( mallocVector
             , unsafeFromForeignPtr0
             , unsafeToForeignPtr0
             ) where

------------------------------------------------------------------------
-- Imports
------------------------------------------------------------------------

-- from base:
import Prelude            ( (*), undefined )
import Data.Int           ( Int )
import Foreign.Storable   ( Storable, sizeOf )
import Foreign.ForeignPtr ( ForeignPtr )
import System.IO          ( IO )

#if __GLASGOW_HASKELL__ >= 605
import GHC.ForeignPtr     ( mallocPlainForeignPtrBytes )
#endif

-- from vector:
import Data.Vector.Storable ( Vector
                            , unsafeFromForeignPtr
                            , unsafeToForeignPtr
                            )

------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------

mallocVector :: Storable a => Int -> IO (ForeignPtr a)
mallocVector =
#if __GLASGOW_HASKELL__ >= 605
    doMalloc undefined
        where
          doMalloc :: Storable b => b -> Int -> IO (ForeignPtr b)
          doMalloc dummy size = mallocPlainForeignPtrBytes (size * sizeOf dummy)
#else
    mallocForeignPtrArray
#endif
{-# INLINE mallocVector #-}

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
