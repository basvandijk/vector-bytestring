{-# LANGUAGE CPP
           , NoImplicitPrelude
           , TypeSynonymInstances
           , FlexibleInstances
           , BangPatterns
           , MagicHash
           , ForeignFunctionInterface
  #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif

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

        -- * Conversion with lists: packing and unpacking
        packBytes, packUptoLenBytes, unsafePackLenBytes,
        packChars, packUptoLenChars, unsafePackLenChars,
        unpackBytes, unpackAppendBytesLazy, unpackAppendBytesStrict,
        unpackChars, unpackAppendCharsLazy, unpackAppendCharsStrict,

        -- * Low level introduction and elimination
        create,                 -- :: Int -> (Ptr Word8 -> IO ()) -> IO ByteString
        createUptoN,            -- :: Int -> (Ptr Word8 -> IO Int) -> IO ByteString

        createAndTrim,          -- :: Int -> (Ptr Word8 -> IO Int) -> IO  ByteString
        createAndTrim',         -- :: Int -> (Ptr Word8 -> IO (Int, Int, a)) -> IO (ByteString, a)
        unsafeCreate,           -- :: Int -> (Ptr Word8 -> IO ()) ->  ByteString
        unsafeCreateUptoN,      -- :: Int -> (Ptr Word8 -> IO Int) ->  ByteString
        mallocByteString,       -- :: Int -> IO (ForeignPtr a)

        -- * Conversion to and from ForeignPtrs
        fromForeignPtr,         -- :: ForeignPtr Word8 -> Int -> Int -> ByteString
        toForeignPtr,           -- :: ByteString -> (ForeignPtr Word8, Int, Int)

        -- * Utilities
        inlinePerformIO,        -- :: IO a -> a
        nullForeignPtr,         -- :: ForeignPtr Word8

        -- * Standard C Functions
        c_strlen,               -- :: CString -> IO CInt
        c_free_finalizer,       -- :: FunPtr (Ptr Word8 -> IO ())

        memchr,                 -- :: Ptr Word8 -> Word8 -> CSize -> IO Ptr Word8
        memcmp,                 -- :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt
        memcpy,                 -- :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
        memset,                 -- :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)

        -- * cbits functions
        c_reverse,              -- :: Ptr Word8 -> Ptr Word8 -> CInt -> IO ()
        c_intersperse,          -- :: Ptr Word8 -> Ptr Word8 -> CInt -> Word8 -> IO ()
        c_maximum,              -- :: Ptr Word8 -> CInt -> IO Word8
        c_minimum,              -- :: Ptr Word8 -> CInt -> IO Word8
        c_count,                -- :: Ptr Word8 -> CInt -> Word8 -> IO CInt

        -- * Chars
        w2c, c2w, isSpaceWord8, isSpaceChar8

  ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Exception  ( assert )
import Control.Monad      ( (>>), return, void )
import Data.Char          ( Char, ord )
import Data.Bool          ( Bool, (||) )
import Data.Eq            ( (==) )
import Data.Function      ( (.) )
import Data.Ord           ( (<=), (>=) )
import Data.Word          ( Word8 )
import Foreign.C.String   ( CString )
import Foreign.Ptr        ( FunPtr )
import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr  )
import Foreign.Ptr        ( Ptr, plusPtr )
import Foreign.Storable   ( peek, poke )
import Prelude            ( Int, (-), ($), ($!), fromIntegral, otherwise, undefined )
import System.IO          ( IO )
-- import Text.Read          ( Read, readsPrec )
-- import Text.Show          ( Show, showsPrec )

import qualified Data.List as List

#if __GLASGOW_HASKELL__ >= 704
import Foreign.C.Types    ( CSize(..), CInt(..), CULong(..) )
#else
import Foreign.C.Types    ( CSize,     CInt,     CULong     )
#endif

#if __GLASGOW_HASKELL__ >= 702
import System.IO.Unsafe  ( unsafeDupablePerformIO )
#else
import GHC.IO            ( unsafeDupablePerformIO )
#endif

import GHC.ForeignPtr    ( ForeignPtr(ForeignPtr), mallocPlainForeignPtrBytes )
import GHC.Base          ( nullAddr#, unsafeChr )

-- from primitive:
import Control.Monad.Primitive ( unsafeInlineIO )

-- from vector:
import qualified Data.Vector.Storable as VS


--------------------------------------------------------------------------------
-- The ByteString type synonym
--------------------------------------------------------------------------------

-- | A space-efficient representation of a 'Word8' vector, supporting many
-- efficient operations.  A 'ByteString' contains 8-bit characters only.
type ByteString = VS.Vector Word8

{-
-- TODO: Probably not a good idea to add these orphaned instances:
--------------------------------------------------------------------------------
-- Show & Read instances
--------------------------------------------------------------------------------

instance Show ByteString where
    showsPrec p ps r = showsPrec p (unpackWith w2c ps) r

instance Read ByteString where
    readsPrec p str = [ (packWith c2w x, y) | (x, y) <- readsPrec p str ]

-- | /O(n)/ Converts a 'ByteString' to a '[a]', using a conversion function.
unpackWith :: (Word8 -> a) -> ByteString -> [a]
unpackWith k v
    | l == 0 = []
    | otherwise =
        unsafeInlineIO $ withForeignPtr fp $ \p ->
          let go 0  !acc = peek p          >>= \e -> return   (k e : acc)
              go !n !acc = peekByteOff p n >>= \e -> go (n-1) (k e : acc)
          in go (l - 1) []
    where
      (fp, l) = unsafeToForeignPtr0 v
{-# INLINE unpackWith #-}

-- | /O(n)/ Convert a '[a]' into a 'ByteString' using some
-- conversion function
packWith :: (a -> Word8) -> [a] -> ByteString
packWith k str = unsafeCreate (length str) $ \p -> go p str
    where
        go _  []     = return ()
        go !p (x:xs) = poke p (k x) >> go (p `plusPtr` 1) xs
                       -- less space than pokeElemOff
{-# INLINE packWith #-}
-}

------------------------------------------------------------------------
-- Packing and unpacking from lists

packBytes :: [Word8] -> ByteString
packBytes ws = unsafePackLenBytes (List.length ws) ws

packChars :: [Char] -> ByteString
packChars cs = unsafePackLenChars (List.length cs) cs

unsafePackLenBytes :: Int -> [Word8] -> ByteString
unsafePackLenBytes len xs0 =
    unsafeCreate len $ \p -> go p xs0
  where
    go !_ []     = return ()
    go !p (x:xs) = poke p x >> go (p `plusPtr` 1) xs

unsafePackLenChars :: Int -> [Char] -> ByteString
unsafePackLenChars len cs0 =
    unsafeCreate len $ \p -> go p cs0
  where
    go !_ []     = return ()
    go !p (c:cs) = poke p (c2w c) >> go (p `plusPtr` 1) cs

packUptoLenBytes :: Int -> [Word8] -> (ByteString, [Word8])
packUptoLenBytes len xs0 =
    unsafeCreateUptoN' len $ \p -> go p len xs0
  where
    go !_ !n []     = return (len-n, [])
    go !_ !0 xs     = return (len,   xs)
    go !p !n (x:xs) = poke p x >> go (p `plusPtr` 1) (n-1) xs

packUptoLenChars :: Int -> [Char] -> (ByteString, [Char])
packUptoLenChars len cs0 =
    unsafeCreateUptoN' len $ \p -> go p len cs0
  where
    go !_ !n []     = return (len-n, [])
    go !_ !0 cs     = return (len,   cs)
    go !p !n (c:cs) = poke p (c2w c) >> go (p `plusPtr` 1) (n-1) cs

-- Unpacking bytestrings into lists effeciently is a tradeoff: on the one hand
-- we would like to write a tight loop that just blats the list into memory, on
-- the other hand we want it to be unpacked lazily so we don't end up with a
-- massive list data structure in memory.
--
-- Our strategy is to combine both: we will unpack lazily in reasonable sized
-- chunks, where each chunk is unpacked strictly.
--
-- unpackBytes and unpackChars do the lazy loop, while unpackAppendBytes and
-- unpackAppendChars do the chunks strictly.

unpackBytes :: ByteString -> [Word8]
unpackBytes bs = unpackAppendBytesLazy bs []

unpackChars :: ByteString -> [Char]
unpackChars bs = unpackAppendCharsLazy bs []

unpackAppendBytesLazy :: ByteString -> [Word8] -> [Word8]
unpackAppendBytesLazy v xs
  | VS.length v <= 100 = unpackAppendBytesStrict v      xs
  | otherwise          = unpackAppendBytesStrict before remainder
  where
    (before, after) = VS.splitAt 100 v
    remainder = unpackAppendBytesLazy after xs

  -- Why 100 bytes you ask? Because on a 64bit machine the list we allocate
  -- takes just shy of 4k which seems like a reasonable amount.
  -- (5 words per list element, 8 bytes per word, 100 elements = 4000 bytes)

unpackAppendCharsLazy :: ByteString -> [Char] -> [Char]
unpackAppendCharsLazy v cs
  | VS.length v <= 100 = unpackAppendCharsStrict v      cs
  | otherwise          = unpackAppendCharsStrict before remainder
  where
    (before, after) = VS.splitAt 100 v
    remainder = unpackAppendCharsLazy after cs

-- For these unpack functions, since we're unpacking the whole list strictly we
-- build up the result list in an accumulator. This means we have to build up
-- the list starting at the end. So our traversal starts at the end of the
-- buffer and loops down until we hit the sentinal:

unpackAppendBytesStrict :: ByteString -> [Word8] -> [Word8]
unpackAppendBytesStrict v xs =
    unsafeInlineIO $ withForeignPtr fp $ \base ->
      let sentinal = base `plusPtr` (-1)
          loop !p acc
              | p == sentinal = return acc
              | otherwise     = do x <- peek p
                                   loop (p `plusPtr` (-1)) (x:acc)
      in loop (base `plusPtr` (l-1)) xs
  where
    (fp, l) = VS.unsafeToForeignPtr0 v

unpackAppendCharsStrict :: ByteString -> [Char] -> [Char]
unpackAppendCharsStrict v xs =
    unsafeInlineIO $ withForeignPtr fp $ \base ->
      let sentinal = base `plusPtr` (-1)
          loop !p acc
              | p == sentinal = return acc
              | otherwise     = do x <- peek p
                                   loop (p `plusPtr` (-1)) (w2c x:acc)
      in loop (base `plusPtr` (l-1)) xs
  where
    (fp, l) = VS.unsafeToForeignPtr0 v

--------------------------------------------------------------------------------
-- * Low level introduction and elimination
--------------------------------------------------------------------------------

-- | Create ByteString of size @l@ and use action @f@ to fill it's contents.
create :: Int -> (Ptr Word8 -> IO ()) -> IO ByteString
create l f = do
  fp <- mallocByteString l
  withForeignPtr fp $ \p -> do
    f p
    return $! VS.unsafeFromForeignPtr0 fp l
{-# INLINE create #-}

-- | Create ByteString of up to size size @l@ and use action @f@ to fill it's
-- contents which returns its true size.
createUptoN :: Int -> (Ptr Word8 -> IO Int) -> IO ByteString
createUptoN l f = do
    fp <- mallocByteString l
    l' <- withForeignPtr fp $ \p -> f p
    assert (l' <= l) $ return $! VS.unsafeFromForeignPtr0 fp l'
{-# INLINE createUptoN #-}

-- | Create ByteString of up to size @l@ and use action @f@ to fill it's contents which returns its true size.
createUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> IO (ByteString, a)
createUptoN' l f = do
    fp <- mallocByteString l
    (l', res) <- withForeignPtr fp $ \p -> f p
    assert (l' <= l) $ return (VS.unsafeFromForeignPtr0 fp l', res)
{-# INLINE createUptoN' #-}

-- | A way of creating ByteStrings outside the IO monad. The @Int@
-- argument gives the final size of the ByteString. Unlike
-- 'createAndTrim' the ByteString is not reallocated if the final size
-- is less than the estimated size.
unsafeCreate :: Int -> (Ptr Word8 -> IO ()) -> ByteString
unsafeCreate l f = unsafeDupablePerformIO (create l f)
{-# INLINE unsafeCreate #-}

-- | Like 'unsafeCreate' but instead of giving the final size of the
-- ByteString, it is just an upper bound. The inner action returns
-- the actual size. Unlike 'createAndTrim' the ByteString is not
-- reallocated if the final size is less than the estimated size.
unsafeCreateUptoN :: Int -> (Ptr Word8 -> IO Int) -> ByteString
unsafeCreateUptoN l f = unsafeDupablePerformIO (createUptoN l f)
{-# INLINE unsafeCreateUptoN #-}

unsafeCreateUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> (ByteString, a)
unsafeCreateUptoN' l f = unsafeDupablePerformIO (createUptoN' l f)
{-# INLINE unsafeCreateUptoN' #-}

-- | Wrapper of 'mallocForeignPtrBytes' with faster implementation for GHC.
mallocByteString :: Int -> IO (ForeignPtr a)
mallocByteString = mallocPlainForeignPtrBytes
{-# INLINE mallocByteString #-}

-- | Given the maximum size needed and a function to make the contents
-- of a ByteString, createAndTrim makes the 'ByteString'. The generating
-- function is required to return the actual final size (<= the maximum
-- size), and the resulting byte array is realloced to this size.
--
-- createAndTrim is the main mechanism for creating custom, efficient
-- ByteString functions, using Haskell or C functions to fill the space.
createAndTrim :: Int -> (Ptr Word8 -> IO Int) -> IO ByteString
createAndTrim l f = do
  fp <- mallocByteString l
  withForeignPtr fp $ \p -> do
    l' <- f p
    if assert (l' <= l) $ l' >= l
      then return $! VS.unsafeFromForeignPtr0 fp l
      else create l' $ \p' -> memcpy p' p (fromIntegral l')
{-# INLINE createAndTrim #-}

createAndTrim' :: Int -> (Ptr Word8 -> IO (Int, Int, a)) -> IO (ByteString, a)
createAndTrim' l f = do
  fp <- mallocByteString l
  withForeignPtr fp $ \p -> do
    (off, l', res) <- f p
    if assert (l' <= l) $ l' >= l
      then return $! (VS.unsafeFromForeignPtr0 fp l, res)
      else do v <- create l' $ \p' ->
                     memcpy p' (p `plusPtr` off) (fromIntegral l')
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

-- | The 0 pointer. Used to indicate the empty Bytestring.
nullForeignPtr :: ForeignPtr Word8
nullForeignPtr = ForeignPtr nullAddr# undefined --TODO: should ForeignPtrContents be strict?


--------------------------------------------------------------------------------
-- * Standard C Functions
--------------------------------------------------------------------------------

foreign import ccall unsafe "string.h strlen" c_strlen
    :: CString -> IO CSize

foreign import ccall unsafe "static stdlib.h &free" c_free_finalizer
    :: FunPtr (Ptr Word8 -> IO ())

foreign import ccall unsafe "string.h memchr" c_memchr
    :: Ptr Word8 -> CInt -> CSize -> IO (Ptr Word8)

memchr :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)
memchr p w s = c_memchr p (fromIntegral w) s

foreign import ccall unsafe "string.h memcmp" c_memcmp
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

memcmp :: Ptr Word8 -> Ptr Word8 -> Int -> IO CInt
memcmp p q s = c_memcmp p q (fromIntegral s)

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

memcpy :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memcpy p q s = void $ c_memcpy p q (fromIntegral s)

foreign import ccall unsafe "string.h memset" c_memset
    :: Ptr Word8 -> CInt -> CSize -> IO (Ptr Word8)

memset :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)
memset p w s = c_memset p (fromIntegral w) s


--------------------------------------------------------------------------------
-- * cbits functions
--------------------------------------------------------------------------------

foreign import ccall unsafe "static bytestring.h bytestring_reverse" c_reverse
    :: Ptr Word8 -> Ptr Word8 -> CULong -> IO ()

foreign import ccall unsafe "static bytestring.h bytestring_intersperse" c_intersperse
    :: Ptr Word8 -> Ptr Word8 -> CULong -> Word8 -> IO ()

foreign import ccall unsafe "static bytestring.h bytestring_maximum" c_maximum
    :: Ptr Word8 -> CULong -> IO Word8

foreign import ccall unsafe "static bytestring.h bytestring_minimum" c_minimum
    :: Ptr Word8 -> CULong -> IO Word8

foreign import ccall unsafe "static bytestring.h bytestring_count" c_count
    :: Ptr Word8 -> CULong -> Word8 -> IO CULong


--------------------------------------------------------------------------------
-- * Chars
--------------------------------------------------------------------------------

-- | Conversion between 'Word8' and 'Char'. Should compile to a no-op.
w2c :: Word8 -> Char
w2c = unsafeChr . fromIntegral
{-# INLINE w2c #-}

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for ByteString construction.
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

-- | Selects words corresponding to white-space characters in the Latin-1 range
-- ordered by frequency.
isSpaceWord8 :: Word8 -> Bool
isSpaceWord8 w =
    w == 0x20 ||
    w == 0x0A || -- LF, \n
    w == 0x09 || -- HT, \t
    w == 0x0C || -- FF, \f
    w == 0x0D || -- CR, \r
    w == 0x0B || -- VT, \v
    w == 0xA0    -- spotted by QC..
{-# INLINE isSpaceWord8 #-}

-- | Selects white-space characters in the Latin-1 range
isSpaceChar8 :: Char -> Bool
isSpaceChar8 c =
    c == ' '     ||
    c == '\t'    ||
    c == '\n'    ||
    c == '\r'    ||
    c == '\f'    ||
    c == '\v'    ||
    c == '\xa0'
{-# INLINE isSpaceChar8 #-}
