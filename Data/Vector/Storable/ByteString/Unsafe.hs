{-# LANGUAGE NoImplicitPrelude, MagicHash #-}

-- |
-- Module      : Data.Vector.Storable.ByteString.Unsafe
-- License     : BSD-style
-- Maintainer  : Bas van Dijk <v.dijk.bas@gmail.com>
-- Stability   : experimental
--
-- A module containing unsafe 'ByteString' operations.
--
-- While these functions have a stable API and you may use these functions in
-- applications, do carefully consider the documented pre-conditions;
-- incorrect use can break referential transparency or worse.
--
module Data.Vector.Storable.ByteString.Unsafe (

        -- * Unchecked access
        unsafeHead,             -- :: ByteString -> Word8
        unsafeTail,             -- :: ByteString -> ByteString
        unsafeIndex,            -- :: ByteString -> Int -> Word8
        unsafeTake,             -- :: Int -> ByteString -> ByteString
        unsafeDrop,             -- :: Int -> ByteString -> ByteString

        -- * Low level interaction with CStrings
        -- ** Using ByteStrings with functions for CStrings
        unsafeUseAsCString,     -- :: ByteString -> (CString -> IO a) -> IO a
        unsafeUseAsCStringLen,  -- :: ByteString -> (CStringLen -> IO a) -> IO a

        -- ** Converting CStrings to ByteStrings
        unsafePackCString,      -- :: CString -> IO ByteString
        unsafePackCStringLen,   -- :: CStringLen -> IO ByteString
        unsafePackMallocCString,-- :: CString -> IO ByteString

        unsafePackAddress,          -- :: Addr# -> IO ByteString
        unsafePackAddressLen,       -- :: Int -> Addr# -> IO ByteString
        unsafePackCStringFinalizer, -- :: Ptr Word8 -> Int -> IO () -> IO ByteString
        unsafeFinalize,             -- :: ByteString -> IO ()

  ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Word          ( Word8 )
import Data.Function      ( (.) )
import Foreign.Ptr        ( castPtr )
import Foreign.ForeignPtr ( newForeignPtr, withForeignPtr )
import Foreign.C.String   ( CString, CStringLen )
import System.IO          ( IO )
import Prelude            ( Int, fromIntegral, ($), ($!) )
import Control.Monad      ( return )

import Foreign.ForeignPtr ( newForeignPtr_, finalizeForeignPtr )
import qualified Foreign.Concurrent as FC ( newForeignPtr )
import GHC.Prim           ( Addr# )
import GHC.Ptr            ( Ptr(..) )

-- from bytestring:
import qualified Data.ByteString.Internal as BI

-- from vector:
import qualified Data.Vector.Storable as VS

-- from vector-bytestring (this package):
import Data.Vector.Storable.ByteString.Internal ( ByteString )
import Utils ( unsafeToForeignPtr0, unsafeFromForeignPtr0 )


--------------------------------------------------------------------------------
-- * Unchecked access
--------------------------------------------------------------------------------

-- | A variety of 'head' for non-empty ByteStrings. 'unsafeHead' omits the
-- check for the empty case, so there is an obligation on the programmer
-- to provide a proof that the ByteString is non-empty.
unsafeHead :: ByteString -> Word8
unsafeHead = VS.unsafeHead
{-# INLINE unsafeHead #-}

-- | A variety of 'tail' for non-empty ByteStrings. 'unsafeTail' omits the
-- check for the empty case. As with 'unsafeHead', the programmer must
-- provide a separate proof that the ByteString is non-empty.
unsafeTail :: ByteString -> ByteString
unsafeTail = VS.unsafeTail
{-# INLINE unsafeTail #-}

-- | Unsafe 'ByteString' index (subscript) operator, starting from 0, returning a 'Word8'
-- This omits the bounds check, which means there is an accompanying
-- obligation on the programmer to ensure the bounds are checked in some
-- other way.
unsafeIndex :: ByteString -> Int -> Word8
unsafeIndex = VS.unsafeIndex
{-# INLINE unsafeIndex #-}

-- | A variety of 'take' which omits the checks on @n@ so there is an
-- obligation on the programmer to provide a proof that @0 <= n <= 'length' xs@.
unsafeTake :: Int -> ByteString -> ByteString
unsafeTake = VS.unsafeTake
{-# INLINE unsafeTake #-}

-- | A variety of 'drop' which omits the checks on @n@ so there is an
-- obligation on the programmer to provide a proof that @0 <= n <= 'length' xs@.
unsafeDrop  :: Int -> ByteString -> ByteString
unsafeDrop = VS.unsafeDrop
{-# INLINE unsafeDrop #-}


--------------------------------------------------------------------------------
-- * Low level interaction with CStrings
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Using ByteStrings with functions for CStrings

-- | /O(1) construction/ Use a @ByteString@ with a function requiring a
-- @CString@.
--
-- This function does zero copying, and merely unwraps a @ByteString@ to
-- appear as a @CString@. It is /unsafe/ in two ways:
--
-- * After calling this function the @CString@ shares the underlying
-- byte buffer with the original @ByteString@. Thus modifying the
-- @CString@, either in C, or using poke, will cause the contents of the
-- @ByteString@ to change, breaking referential transparency. Other
-- @ByteStrings@ created by sharing (such as those produced via 'take'
-- or 'drop') will also reflect these changes. Modifying the @CString@
-- will break referential transparency. To avoid this, use
-- @useAsCString@, which makes a copy of the original @ByteString@.
--
-- * @CStrings@ are often passed to functions that require them to be
-- null-terminated. If the original @ByteString@ wasn't null terminated,
-- neither will the @CString@ be. It is the programmers responsibility
-- to guarantee that the @ByteString@ is indeed null terminated. If in
-- doubt, use @useAsCString@.
--
unsafeUseAsCString :: ByteString -> (CString -> IO a) -> IO a
unsafeUseAsCString v ac = VS.unsafeWith v $ ac . castPtr
{-# INLINE unsafeUseAsCString #-}

-- | /O(1) construction/ Use a @ByteString@ with a function requiring a
-- @CStringLen@.
--
-- This function does zero copying, and merely unwraps a @ByteString@ to
-- appear as a @CStringLen@. It is /unsafe/:
--
-- * After calling this function the @CStringLen@ shares the underlying
-- byte buffer with the original @ByteString@. Thus modifying the
-- @CStringLen@, either in C, or using poke, will cause the contents of the
-- @ByteString@ to change, breaking referential transparency. Other
-- @ByteStrings@ created by sharing (such as those produced via 'take'
-- or 'drop') will also reflect these changes. Modifying the @CStringLen@
-- will break referential transparency. To avoid this, use
-- @useAsCStringLen@, which makes a copy of the original @ByteString@.
--
unsafeUseAsCStringLen :: ByteString -> (CStringLen -> IO a) -> IO a
unsafeUseAsCStringLen v f = withForeignPtr fp $ \p -> f (castPtr p, l)
    where
      (fp, l) = unsafeToForeignPtr0 v
{-# INLINE unsafeUseAsCStringLen #-}

--------------------------------------------------------------------------------
--  ** Converting CStrings to ByteStrings

-- | /O(n)/ Build a @ByteString@ from a @CString@. This value will have /no/
-- finalizer associated to it, and will not be garbage collected by
-- Haskell. The ByteString length is calculated using /strlen(3)/,
-- and thus the complexity is a /O(n)/.
--
-- This function is /unsafe/. If the @CString@ is later modified, this
-- change will be reflected in the resulting @ByteString@, breaking
-- referential transparency.
--
unsafePackCString :: CString -> IO ByteString
unsafePackCString cstr = do
    let p = castPtr cstr
    fp <- newForeignPtr_ p
    l <- BI.c_strlen cstr
    return $! unsafeFromForeignPtr0 fp (fromIntegral l)
{-# INLINE unsafePackCString #-}

-- | /O(1)/ Build a @ByteString@ from a @CStringLen@. This value will
-- have /no/ finalizer associated with it, and will not be garbage
-- collected by Haskell. This operation has /O(1)/ complexity as we
-- already know the final size, so no /strlen(3)/ is required.
--
-- This funtion is /unsafe/. If the original @CStringLen@ is later
-- modified, this change will be reflected in the resulting @ByteString@,
-- breaking referential transparency.
--
unsafePackCStringLen :: CStringLen -> IO ByteString
unsafePackCStringLen (ptr, l) = do
    let p = castPtr ptr
    fp <- newForeignPtr_ p
    return $! unsafeFromForeignPtr0 fp (fromIntegral l)
{-# INLINE unsafePackCStringLen #-}

-- | /O(n)/ Build a @ByteString@ from a malloced @CString@. This value will
-- have a @free(3)@ finalizer associated to it.
--
-- This funtion is /unsafe/. If the original @CString@ is later
-- modified, this change will be reflected in the resulting @ByteString@,
-- breaking referential transparency.
--
-- This function is also unsafe if you call its finalizer twice,
-- which will result in a /double free/ error, or if you pass it
-- a CString not allocated with 'malloc'.
--
unsafePackMallocCString :: CString -> IO ByteString
unsafePackMallocCString cstr = do
    let p = castPtr cstr
    fp <- newForeignPtr BI.c_free_finalizer p
    l <- BI.c_strlen cstr
    return $! unsafeFromForeignPtr0 fp (fromIntegral l)
{-# INLINE unsafePackMallocCString #-}

-- | /O(n)/ Pack a null-terminated sequence of bytes, pointed to by an
-- Addr\# (an arbitrary machine address assumed to point outside the
-- garbage-collected heap) into a @ByteString@. A much faster way to
-- create an Addr\# is with an unboxed string literal, than to pack a
-- boxed string. A unboxed string literal is compiled to a static @char
-- []@ by GHC. Establishing the length of the string requires a call to
-- @strlen(3)@, so the Addr# must point to a null-terminated buffer (as
-- is the case with "string"# literals in GHC). Use 'unsafePackAddressLen'
-- if you know the length of the string statically.
--
-- An example:
--
-- > literalFS = unsafePackAddress "literal"#
--
-- This function is /unsafe/. If you modify the buffer pointed to by the
-- original Addr# this modification will be reflected in the resulting
-- @ByteString@, breaking referential transparency.
--
-- Note this also won't work if your Addr# has embedded '\0' characters in
-- the string (strlen will fail).
--
unsafePackAddress :: Addr# -> IO ByteString
unsafePackAddress addr# = do
    let cstr :: CString
        cstr = Ptr addr#

        p :: Ptr Word8
        p = castPtr cstr

    fp <- newForeignPtr_ p
    l <- BI.c_strlen cstr
    return $! unsafeFromForeignPtr0 fp (fromIntegral l)
{-# INLINE unsafePackAddress #-}

-- | /O(1)/ 'unsafePackAddressLen' provides constant-time construction of
-- 'ByteStrings' which is ideal for string literals. It packs a sequence
-- of bytes into a 'ByteString', given a raw 'Addr#' to the string, and
-- the length of the string.
--
-- This function is /unsafe/ in two ways:
--
-- * the length argument is assumed to be correct. If the length
-- argument is incorrect, it is possible to overstep the end of the
-- byte array.
--
-- * if the underying Addr# is later modified, this change will be
-- reflected in resulting @ByteString@, breaking referential
-- transparency.
--
-- If in doubt, don't use these functions.
--
unsafePackAddressLen :: Int -> Addr# -> IO ByteString
unsafePackAddressLen l addr# = do
    let p :: Ptr Word8
        p = Ptr addr#
    fp <- newForeignPtr_ p
    return $! unsafeFromForeignPtr0 fp (fromIntegral l)
{-# INLINE unsafePackAddressLen #-}

-- | /O(1)/ Construct a 'ByteString' given a Ptr Word8 to a buffer, a
-- length, and an IO action representing a finalizer. This function is
-- not available on Hugs.
--
-- This function is /unsafe/, it is possible to break referential
-- transparency by modifying the underlying buffer pointed to by the
-- first argument. Any changes to the original buffer will be reflected
-- in the resulting @ByteString@.
--
unsafePackCStringFinalizer :: Ptr Word8 -> Int -> IO () -> IO ByteString
unsafePackCStringFinalizer p l f = do
    fp <- FC.newForeignPtr p f
    return $! unsafeFromForeignPtr0 fp (fromIntegral l)
{-# INLINE unsafePackCStringFinalizer #-}

-- | Explicitly run the finaliser associated with a 'ByteString'.
-- References to this value after finalisation may generate invalid memory
-- references.
--
-- This function is /unsafe/, as there may be other
-- 'ByteStrings' referring to the same underlying pages. If you use
-- this, you need to have a proof of some kind that all 'ByteString's
-- ever generated from the underlying byte array are no longer live.
--
unsafeFinalize :: ByteString -> IO ()
unsafeFinalize v = let (fp, _) = unsafeToForeignPtr0 v
                   in finalizeForeignPtr fp
{-# INLINE unsafeFinalize #-}
