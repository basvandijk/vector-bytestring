{-# LANGUAGE CPP
           , NoImplicitPrelude
           , TypeSynonymInstances
           , FlexibleInstances
           , BangPatterns
  #-}

-- |
-- Module      : Data.Vector.Storable.ByteString.Internal
-- License     : BSD-style
-- Maintainer  : Bas van Dijk <v.dijk.bas@gmail.com>
-- Stability   : experimental
--
-- A module containing semi-public 'ByteString' internals. This exposes the
-- 'ByteString' representation and low level construction functions. As such
-- all the functions in this module are unsafe. The API is also not stable.
--
-- Where possible application should instead use the functions from the normal
-- public interface modules, such as "Data.Vector.Storable.ByteString.Unsafe".
-- Packages that extend the ByteString system at a low level will need to use
-- this module.
--
module Data.Vector.Storable.ByteString.Internal (

        -- * The @ByteString@ type and representation
        ByteString,         -- instances: Eq, Ord, Show, Read, Data, Typeable

        -- * Low level introduction and elimination
        create,                 -- :: Int -> (Ptr Word8 -> IO ()) -> IO ByteString

        createAndTrim,          -- :: Int -> (Ptr Word8 -> IO Int) -> IO  ByteString
        createAndTrim',         -- :: Int -> (Ptr Word8 -> IO (Int, Int, a)) -> IO (ByteString, a)
        unsafeCreate,           -- :: Int -> (Ptr Word8 -> IO ()) ->  ByteString
        BI.mallocByteString,    -- :: Int -> IO (ForeignPtr a)

        -- * Conversion to and from ForeignPtrs
        fromForeignPtr,         -- :: ForeignPtr Word8 -> Int -> Int -> ByteString
        toForeignPtr,           -- :: ByteString -> (ForeignPtr Word8, Int, Int)


        -- * Utilities
        inlinePerformIO,           -- :: IO a -> a
        BI.nullForeignPtr,         -- :: ForeignPtr Word8

        -- * Standard C Functions
        BI.c_strlen,               -- :: CString -> IO CInt
        BI.c_free_finalizer,       -- :: FunPtr (Ptr Word8 -> IO ())

        BI.memchr,                 -- :: Ptr Word8 -> Word8 -> CSize -> IO Ptr Word8
        BI.memcmp,                 -- :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt
        BI.memcpy,                 -- :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
        BI.memset,                 -- :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)

        -- * cbits functions
        BI.c_reverse,              -- :: Ptr Word8 -> Ptr Word8 -> CInt -> IO ()
        BI.c_intersperse,          -- :: Ptr Word8 -> Ptr Word8 -> CInt -> Word8 -> IO ()
        BI.c_maximum,              -- :: Ptr Word8 -> CInt -> IO Word8
        BI.c_minimum,              -- :: Ptr Word8 -> CInt -> IO Word8
        BI.c_count,                -- :: Ptr Word8 -> CInt -> Word8 -> IO CInt

        -- * Chars
        BI.w2c, BI.c2w, BI.isSpaceWord8, BI.isSpaceChar8

  ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Exception  ( assert )
import Control.Monad      ( return )
import Data.Ord           ( (<=), (>=) )
import Data.Word          ( Word8 )
import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr  )
import Foreign.Ptr        ( Ptr, plusPtr )
import Prelude            ( Int, ($), ($!), fromIntegral )
import System.IO          ( IO )
-- import Text.Read          ( Read, readsPrec )
-- import Text.Show          ( Show, showsPrec )

#if __GLASGOW_HASKELL__ >= 702
import System.IO.Unsafe  ( unsafeDupablePerformIO )
#else
import GHC.IO            ( unsafeDupablePerformIO )
#endif

-- from bytestring:
import qualified Data.ByteString.Internal as BI

-- from primitive:
import Control.Monad.Primitive ( unsafeInlineIO )

-- from vector:
import qualified Data.Vector.Storable as VS

-- from vector-bytestring (this package):
import ForeignPtr ( mallocVector, unsafeFromForeignPtr0 )


--------------------------------------------------------------------------------
-- The ByteString type synonym
--------------------------------------------------------------------------------

-- | A space-efficient representation of a 'Word8' vector, supporting many
-- efficient operations.  A 'ByteString' contains 8-bit characters only.
type ByteString = VS.Vector Word8


{- TODO: Probably not a good idea to add these orphaned instances:
--------------------------------------------------------------------------------
-- Show & Read instances
--------------------------------------------------------------------------------

instance Show ByteString where
    showsPrec p ps r = showsPrec p (unpackWith BI.w2c ps) r
instance Read ByteString where
    readsPrec p str = [ (packWith BI.c2w x, y) | (x, y) <- readsPrec p str ]

-- | /O(n)/ Converts a 'ByteString' to a '[a]', using a conversion function.
unpackWith :: (Word8 -> a) -> ByteString -> [a]
unpackWith k v | l == 0 = []
               | otherwise = unsafeInlineIO $ VS.unsafeWith v $ \p ->
                               go p (l - 1) []
    where
      l = VS.length v
      go !ptr 0  !acc = peek ptr          >>= \e -> return (k e : acc)
      go !ptr !n !acc = peekByteOff ptr n >>= \e -> go ptr (n-1) (k e : acc)
{-# INLINE unpackWith #-}

-- | /O(n)/ Convert a '[a]' into a 'ByteString' using some
-- conversion function
packWith :: (a -> Word8) -> [a] -> ByteString
packWith k str = unsafeCreate (length str) $ \p -> go p str
    where
        go _  []     = return ()
        go !p (x:xs) = poke p (k x) >> go (p `plusPtr` 1) xs -- less space than pokeElemOff
{-# INLINE packWith #-}

-}
--------------------------------------------------------------------------------
-- * Low level introduction and elimination
--------------------------------------------------------------------------------

-- | Create ByteString of size @l@ and use action @f@ to fill it's contents.
create :: Int -> (Ptr Word8 -> IO ()) -> IO ByteString
create l f = do
  fp <- mallocVector l
  withForeignPtr fp $ \p -> do
    f p
    return $! unsafeFromForeignPtr0 fp l
{-# INLINE create #-}

-- | A way of creating ByteStrings outside the IO monad. The @Int@
-- argument gives the final size of the ByteString. Unlike
-- 'createAndTrim' the ByteString is not reallocated if the final size
-- is less than the estimated size.
unsafeCreate :: Int -> (Ptr Word8 -> IO ()) -> ByteString
unsafeCreate l f = unsafeDupablePerformIO (create l f)
{-# INLINE unsafeCreate #-}

-- | Given the maximum size needed and a function to make the contents
-- of a ByteString, createAndTrim makes the 'ByteString'. The generating
-- function is required to return the actual final size (<= the maximum
-- size), and the resulting byte array is realloced to this size.
--
-- createAndTrim is the main mechanism for creating custom, efficient
-- ByteString functions, using Haskell or C functions to fill the space.
createAndTrim :: Int -> (Ptr Word8 -> IO Int) -> IO ByteString
createAndTrim l f = do
  fp <- mallocVector l
  withForeignPtr fp $ \p -> do
    l' <- f p
    if assert (l' <= l) $ l' >= l
      then return $! unsafeFromForeignPtr0 fp l
      else create l' $ \p' -> BI.memcpy p' p (fromIntegral l')
{-# INLINE createAndTrim #-}

createAndTrim' :: Int -> (Ptr Word8 -> IO (Int, Int, a)) -> IO (ByteString, a)
createAndTrim' l f = do
  fp <- mallocVector l
  withForeignPtr fp $ \p -> do
    (off, l', res) <- f p
    if assert (l' <= l) $ l' >= l
      then return $! (unsafeFromForeignPtr0 fp l, res)
      else do v <- create l' $ \p' ->
                     BI.memcpy p' (p `plusPtr` off) (fromIntegral l')
              return $! (v, res)
{-# INLINE createAndTrim' #-}


--------------------------------------------------------------------------------
-- * Conversion to and from ForeignPtrs
--------------------------------------------------------------------------------

-- | /O(1)/ Build a ByteString from a ForeignPtr.
--
-- If you do not need the offset parameter then you do should be using
-- 'Data.Vector.Storable.ByteString.Unsafe.unsafePackCStringLen' or
-- 'Data.Vector.Storable.ByteString.Unsafe.unsafePackCStringFinalizer' instead.
fromForeignPtr :: ForeignPtr Word8
               -> Int -- ^ Offset
               -> Int -- ^ Length
               -> ByteString
fromForeignPtr = VS.unsafeFromForeignPtr
{-# INLINE fromForeignPtr #-}

-- | /O(1)/ Deconstruct a ForeignPtr from a ByteString
toForeignPtr :: ByteString -> (ForeignPtr Word8, Int, Int) -- ^ (ptr, offset, length)
toForeignPtr = VS.unsafeToForeignPtr
{-# INLINE toForeignPtr #-}


--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

-- | Just like unsafePerformIO, but we inline it. Big performance gains as
-- it exposes lots of things to further inlining. /Very unsafe/. In
-- particular, you should do no memory allocation inside an
-- 'inlinePerformIO' block. On Hugs this is just @unsafePerformIO@.
inlinePerformIO :: IO a -> a
inlinePerformIO = unsafeInlineIO
{-# INLINE inlinePerformIO #-}
