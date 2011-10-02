{-# LANGUAGE CPP, NoImplicitPrelude #-}

module ForeignPtr ( mallocVector
                  , unsafeFromForeignPtr0
                  , unsafeToForeignPtr0
                  ) where

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

{-# INLINE mallocVector #-}
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

unsafeFromForeignPtr0 :: Storable a
                      => ForeignPtr a
                      -> Int
                      -> Vector a
{-# INLINE unsafeFromForeignPtr0 #-}
unsafeFromForeignPtr0 fp n = unsafeFromForeignPtr fp 0 n

unsafeToForeignPtr0 :: Storable a => Vector a -> (ForeignPtr a, Int)
{-# INLINE unsafeToForeignPtr0 #-}
unsafeToForeignPtr0 v = let (fp, _, n) = unsafeToForeignPtr v
                        in (fp, n)
