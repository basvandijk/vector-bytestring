{-# LANGUAGE CPP, NoImplicitPrelude #-}

module ForeignPtr ( mallocVector ) where

import Prelude            ( (*), undefined )
import Data.Int           ( Int )
import Foreign.Storable   ( Storable, sizeOf )
import Foreign.ForeignPtr ( ForeignPtr )
import System.IO          ( IO )

#if __GLASGOW_HASKELL__ >= 605
import GHC.ForeignPtr     ( mallocPlainForeignPtrBytes )
#endif

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
