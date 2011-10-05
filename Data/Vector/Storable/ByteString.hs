{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , NamedFieldPuns
           , MagicHash
           , UnboxedTuples
  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
-- Module      : Data.Vector.Storable.ByteString
-- Copyright   : (c) The University of Glasgow 2001,
--               (c) David Roundy 2003-2005,
--               (c) Simon Marlow 2005
--               (c) Bjorn Bringert 2006
--               (c) Don Stewart 2005-2008
--               (c) Bas van Dijk 2011
--
--               Array fusion code:
--               (c) 2001,2002 Manuel M T Chakravarty & Gabriele Keller
--               (c) 2006      Manuel M T Chakravarty & Roman Leshchinskiy
--
-- License     : BSD-style
--
-- Maintainer  : Bas van Dijk <v.dijk.bas@gmail.com>
-- Stability   : experimental
--
-- A time and space-efficient implementation of byte vectors using
-- vectors of Word8, suitable for high performance use, both in terms
-- of large data quantities, or high speed requirements. Byte vectors
-- are encoded as strict 'Word8' arrays of bytes, held in a 'ForeignPtr',
-- and can be passed between C and Haskell with little effort.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.Vector.Storable.ByteString as B
--
-- Original GHC implementation by Bryan O\'Sullivan.
-- Rewritten to use 'Data.Array.Unboxed.UArray' by Simon Marlow.
-- Rewritten to support slices and use 'ForeignPtr' by David Roundy.
-- Polished and extended by Don Stewart.
-- Redefined ByteString as a Vector (from the vector package) of Word8s
-- by Bas van Dijk
--

module Data.Vector.Storable.ByteString (

        -- * The ByteString type
        ByteString,             -- instances: Eq, Ord, Show, Read, Data, Typeable, Monoid

        -- * Introducing and eliminating ByteStrings
        empty,                  -- :: ByteString
        singleton,              -- :: Word8   -> ByteString
        pack,                   -- :: [Word8] -> ByteString
        unpack,                 -- :: ByteString -> [Word8]

        -- * Basic interface
        cons,                   -- :: Word8 -> ByteString -> ByteString
        snoc,                   -- :: ByteString -> Word8 -> ByteString
        append,                 -- :: ByteString -> ByteString -> ByteString

        head,                   -- :: ByteString -> Word8
        uncons,                 -- :: ByteString -> Maybe (Word8, ByteString)

        last,                   -- :: ByteString -> Word8
        tail,                   -- :: ByteString -> ByteString
        init,                   -- :: ByteString -> ByteString
        null,                   -- :: ByteString -> Bool
        length,                 -- :: ByteString -> Int

        -- * Transforming ByteStrings
        map,                    -- :: (Word8 -> Word8) -> ByteString -> ByteString
        reverse,                -- :: ByteString -> ByteString
        intersperse,            -- :: Word8 -> ByteString -> ByteString

        intercalate,            -- :: ByteString -> [ByteString] -> ByteString
        transpose,              -- :: [ByteString] -> [ByteString]

        -- * Reducing ByteStrings (folds)
        foldl,                  -- :: (a -> Word8 -> a) -> a -> ByteString -> a
        foldl',                 -- :: (a -> Word8 -> a) -> a -> ByteString -> a
        foldl1,                 -- :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
        foldl1',                -- :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8

        foldr,                  -- :: (Word8 -> a -> a) -> a -> ByteString -> a
        foldr',                 -- :: (Word8 -> a -> a) -> a -> ByteString -> a
        foldr1,                 -- :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
        foldr1',                -- :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8

        -- ** Special folds
        concat,                 -- :: [ByteString] -> ByteString
        concatMap,              -- :: (Word8 -> ByteString) -> ByteString -> ByteString
        any,                    -- :: (Word8 -> Bool) -> ByteString -> Bool
        all,                    -- :: (Word8 -> Bool) -> ByteString -> Bool
        maximum,                -- :: ByteString -> Word8
        minimum,                -- :: ByteString -> Word8

        -- * Building ByteStrings
        -- ** Scans
        scanl,                  -- :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString
        scanl1,                 -- :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
        scanr,                  -- :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString
        scanr1,                 -- :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString

        -- ** Accumulating maps
        mapAccumL,              -- :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
        mapAccumR,              -- :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)

        -- ** Generating and unfolding ByteStrings
        replicate,              -- :: Int -> Word8 -> ByteString
        unfoldr,                -- :: (a -> Maybe (Word8, a)) -> a -> ByteString
        unfoldrN,               -- :: Int -> (a -> Maybe (Word8, a)) -> a -> (ByteString, Maybe a)

        -- * Substrings

        -- ** Breaking strings
        take,                   -- :: Int -> ByteString -> ByteString
        drop,                   -- :: Int -> ByteString -> ByteString
        splitAt,                -- :: Int -> ByteString -> (ByteString, ByteString)
        takeWhile,              -- :: (Word8 -> Bool) -> ByteString -> ByteString
        dropWhile,              -- :: (Word8 -> Bool) -> ByteString -> ByteString
        span,                   -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
        spanEnd,                -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
        break,                  -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
        breakEnd,               -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
        group,                  -- :: ByteString -> [ByteString]
        groupBy,                -- :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
        inits,                  -- :: ByteString -> [ByteString]
        tails,                  -- :: ByteString -> [ByteString]

        -- ** Breaking into many substrings
        split,                  -- :: Word8 -> ByteString -> [ByteString]
        splitWith,              -- :: (Word8 -> Bool) -> ByteString -> [ByteString]

        -- * Predicates
        isPrefixOf,             -- :: ByteString -> ByteString -> Bool
        isSuffixOf,             -- :: ByteString -> ByteString -> Bool
        isInfixOf,              -- :: ByteString -> ByteString -> Bool

        -- ** Search for arbitrary substrings
        breakSubstring,         -- :: ByteString -> ByteString -> (ByteString,ByteString)
        findSubstring,          -- :: ByteString -> ByteString -> Maybe Int
        findSubstrings,         -- :: ByteString -> ByteString -> [Int]

        -- * Searching ByteStrings

        -- ** Searching by equality
        elem,                   -- :: Word8 -> ByteString -> Bool
        notElem,                -- :: Word8 -> ByteString -> Bool

        -- ** Searching with a predicate
        find,                   -- :: (Word8 -> Bool) -> ByteString -> Maybe Word8
        filter,                 -- :: (Word8 -> Bool) -> ByteString -> ByteString
        partition,              -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)

        -- * Indexing ByteStrings
        index,                  -- :: ByteString -> Int -> Word8
        elemIndex,              -- :: Word8 -> ByteString -> Maybe Int
        elemIndices,            -- :: Word8 -> ByteString -> [Int]
        elemIndexEnd,           -- :: Word8 -> ByteString -> Maybe Int
        findIndex,              -- :: (Word8 -> Bool) -> ByteString -> Maybe Int
        findIndices,            -- :: (Word8 -> Bool) -> ByteString -> [Int]
        count,                  -- :: Word8 -> ByteString -> Int

        -- * Zipping and unzipping ByteStrings
        zip,                    -- :: ByteString -> ByteString -> [(Word8,Word8)]
        zipWith,                -- :: (Word8 -> Word8 -> c) -> ByteString -> ByteString -> [c]
        unzip,                  -- :: [(Word8,Word8)] -> (ByteString,ByteString)

        -- * Ordered ByteStrings
        sort,                   -- :: ByteString -> ByteString

        -- * Low level conversions
        -- ** Copying ByteStrings
        copy,                   -- :: ByteString -> ByteString

        -- ** Packing CStrings and pointers
        packCString,            -- :: CString -> IO ByteString
        packCStringLen,         -- :: CStringLen -> IO ByteString

        -- ** Using ByteStrings as CStrings
        useAsCString,           -- :: ByteString -> (CString    -> IO a) -> IO a
        useAsCStringLen,        -- :: ByteString -> (CStringLen -> IO a) -> IO a

        -- * I\/O with ByteStrings

        -- ** Standard input and output
        getLine,                -- :: IO ByteString
        getContents,            -- :: IO ByteString
        putStr,                 -- :: ByteString -> IO ()
        putStrLn,               -- :: ByteString -> IO ()
        interact,               -- :: (ByteString -> ByteString) -> IO ()

        -- ** Files
        readFile,               -- :: FilePath -> IO ByteString
        writeFile,              -- :: FilePath -> ByteString -> IO ()
        appendFile,             -- :: FilePath -> ByteString -> IO ()

        -- ** I\/O with Handles
        hGetLine,               -- :: Handle -> IO ByteString
        hGetContents,           -- :: Handle -> IO ByteString
        hGet,                   -- :: Handle -> Int -> IO ByteString
        hGetSome,               -- :: Handle -> Int -> IO ByteString
        hGetNonBlocking,        -- :: Handle -> Int -> IO ByteString
        hPut,                   -- :: Handle -> ByteString -> IO ()
        hPutNonBlocking,        -- :: Handle -> ByteString -> IO ByteString
        hPutStr,                -- :: Handle -> ByteString -> IO ()
        hPutStrLn,              -- :: Handle -> ByteString -> IO ()

        breakByte
  ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Exception     ( finally )
import Control.Monad         ( (>>=), (=<<), (>>), return, void, when )
import Data.Bool             ( Bool(False, True), not, otherwise, (||) )
import Data.Char             ( ord )
import Data.Eq               ( (==), (/=) )
import Data.Function         ( (.), flip )
import Data.Functor          ( fmap )
import Data.IORef            ( readIORef, writeIORef )
import Data.Maybe            ( Maybe(Nothing, Just), isJust, listToMaybe )
import Data.Ord              ( min, (<), (>), (<=), (>=) )
import Data.Tuple            ( fst, snd )
import Data.Word             ( Word8 )
import Foreign.C.String      ( CString, CStringLen )
import Foreign.C.Types       ( CSize )
import Foreign.ForeignPtr    ( newForeignPtr, withForeignPtr )
import Foreign.Marshal.Alloc ( allocaBytes, mallocBytes
                             , reallocBytes, finalizerFree
                             )
import Foreign.Marshal.Array ( allocaArray )
import Foreign.Marshal.Utils ( copyBytes )
import Foreign.Ptr           ( nullPtr, plusPtr, minusPtr, castPtr )
import Foreign.Storable      ( peek, poke
                             , peekElemOff, pokeElemOff
                             , peekByteOff, pokeByteOff
                             , sizeOf
                             )
import Prelude               ( (+),(-),(*), ($), ($!)
                             , Int, fromIntegral, String, error, undefined
                             )
import System.IO             ( IO, FilePath, Handle
                             , IOMode(ReadMode, WriteMode, AppendMode)
                             , stdin, stdout
                             , hGetBuf, hGetBufSome, hGetBufNonBlocking
                             , hPutBuf, hPutBufNonBlocking, hFileSize
                             , withBinaryFile, hClose
                             )
import System.IO.Unsafe      ( unsafePerformIO )
import System.IO.Error       ( ioError, mkIOError, illegalOperationErrorType )
import Text.Show             ( show, showsPrec )

import qualified Data.List as L
    ( intersperse, transpose, map, reverse, length )
import Data.List ( (++) )

import GHC.IO.Handle.Internals ( wantReadableHandle_, flushCharReadBuffer
                               , ioe_EOF
                               )
import GHC.IO.Handle.Types     ( Handle__(..) )
import GHC.IO.Buffer           ( RawBuffer, Buffer(Buffer), bufRaw, bufL, bufR
                               , withRawBuffer, isEmptyBuffer, readWord8Buf
                               )
import GHC.IO.BufferedIO as Buffered ( fillReadBuffer )

import GHC.Base                ( build )
import GHC.IO                  ( stToIO )
import GHC.Prim                ( Addr#, plusAddr#, writeWord8OffAddr# )
import GHC.Ptr                 ( Ptr(..) )
import GHC.ST                  ( ST(..) )
import GHC.Word                ( Word8(W8#) )

-- from primitive:
import Control.Monad.Primitive ( unsafeInlineIO )

-- from vector:
import qualified Data.Vector.Storable as VS

-- from vector-bytestring (this package):
import Data.Vector.Storable.ByteString.Internal
    ( ByteString
    , create, unsafeCreate, createAndTrim, createAndTrim'
    , mallocByteString
    , memcpy, memset, memchr, memcmp
    , c_strlen, c_count, c_intersperse
    )
import Utils ( unsafeFromForeignPtr0, unsafeToForeignPtr0 )


--------------------------------------------------------------------------------
-- * Introducing and eliminating 'ByteString's
--------------------------------------------------------------------------------

-- | /O(1)/ The empty 'ByteString'
empty :: ByteString
empty = VS.empty
{-# INLINE empty #-}

-- | /O(1)/ Convert a 'Word8' into a 'ByteString'
singleton :: Word8 -> ByteString
singleton = VS.singleton
{-# INLINE [1] singleton #-} -- Inline [1] for intercalate rule

-- | /O(n)/ Convert a @['Word8']@ into a 'ByteString'.
--
-- For applications with large numbers of string literals, pack can be a
-- bottleneck. In such cases, consider using packAddress (GHC only).
pack :: [Word8] -> ByteString
pack str = unsafeCreate (L.length str) $ \(Ptr p) -> stToIO (go p str)
    where
      go :: Addr# -> [Word8] -> ST a ()
      go _ []           = return ()
      go p (W8# c : cs) = writeByte >> go (p `plusAddr#` 1#) cs
          where
            writeByte = ST $ \s# ->
              case writeWord8OffAddr# p 0# c s# of
                s2# -> (# s2#, () #)
            {-# INLINE writeByte #-}
{-# INLINE pack #-}

-- | /O(n)/ Converts a 'ByteString' to a @['Word8']@.
unpack :: ByteString -> [Word8]
unpack v = build (unpackFoldr v)
{-# INLINE unpack #-}

-- Have unpack fuse with good list consumers
--
-- critical this isn't strict in the acc
-- as it will break in the presence of list fusion. this is a known
-- issue with seq and build/foldr rewrite rules, which rely on lazy
-- demanding to avoid bottoms in the list.
--
unpackFoldr :: ByteString -> (Word8 -> a -> a) -> a -> a
unpackFoldr v f ch = unsafeInlineIO $ withForeignPtr fp $ \p ->
    let go (-1) acc = return acc
        go !n   acc = do
           a <- peekByteOff p n
           go (n-1) (a `f` acc)
    in go (l-1) ch
        where
          (fp, l) = unsafeToForeignPtr0 v
{-# INLINE [0] unpackFoldr #-}

unpackList :: ByteString -> [Word8]
unpackList v = unsafeInlineIO $ withForeignPtr fp $ \p ->
    let go (-1) acc = return acc
        go !n   acc = do
           a <- peekByteOff p n
           go (n-1) (a : acc)
    in go (l-1) []
        where
          (fp, l) = unsafeToForeignPtr0 v
{-# INLINE unpackList #-}

{-# RULES
"ByteString unpack-list" [1]  forall p  .
    unpackFoldr p (:) [] = unpackList p
 #-}


--------------------------------------------------------------------------------
--  * Basic interface
--------------------------------------------------------------------------------

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires a memcpy.
cons :: Word8 -> ByteString -> ByteString
cons = VS.cons
{-# INLINE cons #-}

-- | /O(n)/ Append a byte to the end of a 'ByteString'
snoc :: ByteString -> Word8 -> ByteString
snoc = VS.snoc
{-# INLINE snoc #-}

-- | /O(n)/ Append two ByteStrings
append :: ByteString -> ByteString -> ByteString
append = (VS.++)
{-# INLINE append #-}

-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
-- An exception will be thrown in the case of an empty ByteString.
head :: ByteString -> Word8
head = VS.head
{-# INLINE head #-}

-- | /O(1)/ Extract the elements after the head of a ByteString, which must be non-empty.
-- An exception will be thrown in the case of an empty ByteString.
tail :: ByteString -> ByteString
tail = VS.tail
{-# INLINE tail #-}

-- | /O(1)/ Extract the head and tail of a ByteString, returning Nothing
-- if it is empty.
uncons :: ByteString -> Maybe (Word8, ByteString)
uncons v
    | VS.length v == 0 = Nothing
    | otherwise        = Just (VS.unsafeHead v, VS.unsafeTail v)
{-# INLINE uncons #-}

-- | /O(1)/ Extract the last element of a ByteString, which must be finite and non-empty.
-- An exception will be thrown in the case of an empty ByteString.
last :: ByteString -> Word8
last = VS.last
{-# INLINE last #-}

-- | /O(1)/ Return all the elements of a 'ByteString' except the last one.
-- An exception will be thrown in the case of an empty ByteString.
init :: ByteString -> ByteString
init = VS.init
{-# INLINE init #-}

-- | /O(1)/ Test whether a ByteString is empty.
null :: ByteString -> Bool
null = VS.null
{-# INLINE null #-}

-- | /O(1)/ 'length' returns the length of a ByteString as an 'Int'.
length :: ByteString -> Int
length = VS.length
{-# INLINE length #-}


--------------------------------------------------------------------------------
-- * Transforming ByteStrings
--------------------------------------------------------------------------------

-- | /O(n)/ 'map' @f xs@ is the ByteString obtained by applying @f@ to each
-- element of @xs@. This function is subject to array fusion.
map :: (Word8 -> Word8) -> ByteString -> ByteString
map = VS.map
{-# INLINE map #-}

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
reverse :: ByteString -> ByteString
reverse = VS.reverse
{-# INLINE reverse #-}

-- | /O(n)/ The 'intersperse' function takes a 'Word8' and a
-- 'ByteString' and \`intersperses\' that byte between the elements of
-- the 'ByteString'.  It is analogous to the intersperse function on
-- Lists.
intersperse :: Word8 -> ByteString -> ByteString
intersperse c v
    | l < 2     = v
    | otherwise = unsafeCreate (2*l-1) $ \p' -> withForeignPtr fp $ \p ->
                    c_intersperse p' p (fromIntegral l) c
    where
      (fp, l) = unsafeToForeignPtr0 v
{-# INLINE intersperse #-}

-- | /O(n)/ The 'intercalate' function takes a 'ByteString' and a list of
-- 'ByteString's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: ByteString -> [ByteString] -> ByteString
intercalate s = VS.concat . L.intersperse s
{-# INLINE [1] intercalate #-}

{-# RULES
"ByteString specialise intercalate c -> intercalateByte" forall c s1 s2 .
    intercalate (singleton c) (s1 : s2 : []) = intercalateWithByte c s1 s2
  #-}

-- | /O(n)/ intercalateWithByte. An efficient way to join to two ByteStrings
-- with a char. Around 4 times faster than the generalised join.
intercalateWithByte :: Word8 -> ByteString -> ByteString -> ByteString
intercalateWithByte c v1 v2 =
    unsafeCreate (l1 + l2 + 1) $ \ptr ->
      withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 -> do
          memcpy ptr p1 (fromIntegral l1)
          poke (ptr `plusPtr` l1) c
          memcpy (ptr `plusPtr` (l1 + 1)) p2 (fromIntegral l2)
        where
          (fp1, l1) = unsafeToForeignPtr0 v1
          (fp2, l2) = unsafeToForeignPtr0 v2
{-# INLINE intercalateWithByte #-}

-- | The 'transpose' function transposes the rows and columns of its
-- 'ByteString' argument.
transpose :: [ByteString] -> [ByteString]
transpose = L.map VS.fromList
          . L.transpose
          . L.map VS.toList
{-# INLINE transpose #-}


--------------------------------------------------------------------------------
-- * Reducing 'ByteString's (folds)
--------------------------------------------------------------------------------

foldl :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl = VS.foldl
{-# INLINE foldl #-}

foldl' :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl' = VS.foldl'
{-# INLINE foldl' #-}

foldl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1 = VS.foldl1
{-# INLINE foldl1 #-}

foldl1' :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1' = VS.foldl1'
{-# INLINE foldl1' #-}

foldr :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr = VS.foldr
{-# INLINE foldr #-}

foldr' :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr' = VS.foldr'
{-# INLINE foldr' #-}

foldr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1 = VS.foldr1
{-# INLINE foldr1 #-}

foldr1' :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1' = VS.foldr1'
{-# INLINE foldr1' #-}

--------------------------------------------------------------------------------
-- ** Special folds

-- | /O(n)/ Concatenate a list of ByteStrings.
concat :: [ByteString] -> ByteString
concat = VS.concat
{-# INLINE concat #-}

-- | Map a function over a 'ByteString' and concatenate the results
concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
concatMap = VS.concatMap
{-# INLINE concatMap #-}

-- | /O(n)/ Applied to a predicate and a ByteString, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
any :: (Word8 -> Bool) -> ByteString -> Bool
any = VS.any
{-# INLINE any #-}

-- | /O(n)/ Applied to a predicate and a 'ByteString', 'all' determines
-- if all elements of the 'ByteString' satisfy the predicate.
all :: (Word8 -> Bool) -> ByteString -> Bool
all = VS.all
{-# INLINE all #-}

-- | /O(n)/ 'maximum' returns the maximum value from a 'ByteString'
-- This function will fuse.
-- An exception will be thrown in the case of an empty ByteString.
maximum :: ByteString -> Word8
maximum = VS.maximum
{-# INLINE maximum #-}

-- | /O(n)/ 'minimum' returns the minimum value from a 'ByteString'
-- This function will fuse.
-- An exception will be thrown in the case of an empty ByteString.
minimum :: ByteString -> Word8
minimum = VS.minimum
{-# INLINE minimum #-}


--------------------------------------------------------------------------------
-- * Building ByteStrings
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Scans

-- | 'scanl' is similar to 'foldl', but returns a list of successive
-- reduced values from the left. This function will fuse.
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs.
--
scanl :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString
scanl = VS.scanl
{-# INLINE scanl #-}

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument.
-- This function will fuse.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scanl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanl1 = VS.scanl1
{-# INLINE scanl1 #-}

-- | scanr is the right-to-left dual of scanl.
scanr :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString
scanr = VS.scanr
{-# INLINE scanr #-}

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
scanr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanr1 = VS.scanr1
{-# INLINE scanr1 #-}

--------------------------------------------------------------------------------
-- ** Accumulating maps

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new list.
mapAccumL :: (acc -> Word8 -> (acc, Word8))
          -> acc -> ByteString -> (acc, ByteString)
mapAccumL f acc v = unsafeInlineIO $ withForeignPtr fp $ \p -> do
    fp' <- mallocByteString l
    withForeignPtr fp' $ \p' ->
      let go !a !m
            | m >= l = return (a, unsafeFromForeignPtr0 fp' l)
            | otherwise = do
                x <- peekByteOff p m
                let (a', y) = f a x
                pokeByteOff p' m y
                go a' (m+1)
      in go acc 0
          where
            (fp, l) = unsafeToForeignPtr0 v
{-# INLINE mapAccumL #-}

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from right to left, and returning a
-- final value of this accumulator together with the new ByteString.
mapAccumR :: (acc -> Word8 -> (acc, Word8))
          -> acc -> ByteString -> (acc, ByteString)
mapAccumR f acc v = unsafeInlineIO $ withForeignPtr fp $ \p -> do
    fp' <- mallocByteString l
    withForeignPtr fp' $ \p' ->
      let go !a !m
            | m < 0     = return (a, unsafeFromForeignPtr0 fp' l)
            | otherwise = do
                x <- peekByteOff p m
                let (a', y) = f a x
                pokeByteOff p' m y
                go a' (m-1)
      in go acc (l-1)
          where
            (fp, l) = unsafeToForeignPtr0 v
{-# INLINE mapAccumR #-}

--------------------------------------------------------------------------------
-- ** Generating and unfolding ByteStrings

-- | /O(n)/ 'replicate' @n x@ is a ByteString of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate n x = unfoldr n (\u -> Just (u,u)) x
--
-- This implemenation uses @memset(3)@

-- TODO: Should I use VS.replicate here?
replicate :: Int -> Word8 -> ByteString
replicate n x
    | n <= 0    = VS.empty
    | otherwise = unsafeCreate n $ \p ->
                    void $ memset p x (fromIntegral n)
{-# INLINE replicate #-}

-- | /O(n)/, where /n/ is the length of the result.  The 'unfoldr'
-- function is analogous to the List \'unfoldr\'.  'unfoldr' builds a
-- ByteString from a seed value.  The function takes the element and
-- returns 'Nothing' if it is done producing the ByteString or returns
-- 'Just' @(a,b)@, in which case, @a@ is the next byte in the string,
-- and @b@ is the seed value for further production.
--
-- Examples:
--
-- >    unfoldr (\x -> if x <= 5 then Just (x, x + 1) else Nothing) 0
-- > == pack [0, 1, 2, 3, 4, 5]
--
unfoldr :: (a -> Maybe (Word8, a)) -> a -> ByteString
unfoldr = VS.unfoldr
{-# INLINE unfoldr #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a ByteString from a seed
-- value.  However, the length of the result is limited by the first
-- argument to 'unfoldrN'.  This function is more efficient than 'unfoldr'
-- when the maximum length of the result is known.
--
-- The following equation relates 'unfoldrN' and 'unfoldr':
--
-- > snd (unfoldrN n f s) == take n (unfoldr f s)
--
-- /Note: this function has a different type than @Data.Vector.Storable.'VS.unfoldrN'@!/
unfoldrN :: Int -> (a -> Maybe (Word8, a)) -> a -> (ByteString, Maybe a)
unfoldrN i f x0
    | i < 0     = (VS.empty, Just x0)
    | otherwise = unsafePerformIO $ createAndTrim' i $ \p -> go p x0 0
  where
    go !p !x !n =
        case f x of
          Nothing      -> return (0, n, Nothing)
          Just (w, x')
              | n == i    -> return (0, n, Just x)
              | otherwise -> do
                  poke p w
                  go (p `plusPtr` 1) x' (n+1)
{-# INLINE unfoldrN #-}


--------------------------------------------------------------------------------
-- * Substrings
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Breaking strings

-- | /O(1)/ 'take' @n@, applied to a ByteString @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Int -> ByteString -> ByteString
take = VS.take
{-# INLINE take #-}

-- | /O(1)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or @[]@ if @n > 'length' xs@.
drop  :: Int -> ByteString -> ByteString
drop = VS.drop
{-# INLINE drop #-}

-- | /O(1)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitAt :: Int -> ByteString -> (ByteString, ByteString)
splitAt = VS.splitAt
{-# INLINE splitAt #-}

-- | 'takeWhile', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhile f v = VS.unsafeTake (findIndexOrEnd (not . f) v) v
{-# INLINE takeWhile #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhile f v = VS.unsafeDrop (findIndexOrEnd (not . f) v) v
{-# INLINE dropWhile #-}

-- | 'span' @p xs@ breaks the ByteString into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
span = VS.span
{-# INLINE [1] span #-}

{-# RULES
"ByteString specialise span (x==)" forall x.
    span ((==) x) = spanByte x
"ByteString specialise span (==x)" forall x.
    span (==x) = spanByte x
  #-}

-- | 'spanByte' breaks its ByteString argument at the first
-- occurence of a byte other than its argument. It is more efficient
-- than 'span (==)'
--
-- > span  (=='c') "abcd" == spanByte 'c' "abcd"
--
spanByte :: Word8 -> ByteString -> (ByteString, ByteString)
spanByte c v = unsafeInlineIO $ withForeignPtr fp $ \p ->
  let go !i | i >= l    = return (v, VS.empty)
            | otherwise = do
                c' <- peekByteOff p i
                if c /= c'
                  then return (VS.unsafeTake i v, VS.unsafeDrop i v)
                  else go (i+1)
  in go 0
      where
        (fp, l) = unsafeToForeignPtr0 v
{-# INLINE spanByte #-}

-- | 'spanEnd' behaves like 'span' but from the end of the 'ByteString'.
-- We have
--
-- > spanEnd (not.isSpace) "x y z" == ("x y ","z")
--
-- and
--
-- > spanEnd (not . isSpace) v
-- >    ==
-- > let (x,y) = span (not.isSpace) (reverse v) in (reverse y, reverse x)
--
spanEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd p v = VS.splitAt (findFromEndUntil (not . p) v) v
{-# INLINE spanEnd #-}

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
--
-- Under GHC, a rewrite rule will transform break (==) into a
-- call to the specialised breakByte:
--
-- > break ((==) x) = breakByte x
-- > break (==x) = breakByte x
--
break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
break p v = (VS.unsafeTake n v, VS.unsafeDrop n v)
    where
      !n = findIndexOrEnd p v
{-# INLINE [1] break #-}

-- This RULE LHS is not allowed by ghc-6.4
{-# RULES
"ByteString specialise break (x==)" forall x.
    break ((==) x) = breakByte x
"ByteString specialise break (==x)" forall x.
    break (==x) = breakByte x
  #-}

-- | 'breakByte' breaks its ByteString argument at the first occurence
-- of the specified byte. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
--
-- > break (=='c') "abcd" == breakByte 'c' "abcd"
--
breakByte :: Word8 -> ByteString -> (ByteString, ByteString)
breakByte x v = case VS.elemIndex x v of
    Nothing -> (v, VS.empty)
    Just n  -> (VS.unsafeTake n v, VS.unsafeDrop n v)
{-# INLINE breakByte #-}

-- | 'breakEnd' behaves like 'break' but from the end of the 'ByteString'
--
-- breakEnd p == spanEnd (not.p)
breakEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
breakEnd p v = VS.splitAt (findFromEndUntil p v) v
{-# INLINE breakEnd #-}

-- | The 'group' function takes a ByteString and returns a list of
-- ByteStrings such that the concatenation of the result is equal to the
-- argument.  Moreover, each sublist in the result contains only equal
-- elements.  For example,
--
-- > group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
--
-- It is a special case of 'groupBy', which allows the programmer to
-- supply their own equality test. It is about 40% faster than
-- /groupBy (==)/
group :: ByteString -> [ByteString]
group v
    | VS.null v = []
    | otherwise = ys : group zs
    where
      (ys, zs) = spanByte (VS.unsafeHead v) v
{-# INLINE group #-}

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
groupBy  k v
    | VS.null v = []
    | otherwise = VS.unsafeTake n v : groupBy k (VS.unsafeDrop n v)
    where
      n = 1 + findIndexOrEnd (not . k (VS.unsafeHead v)) (VS.unsafeTail v)
{-# INLINE groupBy #-}

-- | /O(n)/ Return all initial segments of the given 'ByteString', shortest first.
inits :: ByteString -> [ByteString]
inits v = [VS.unsafeTake s v | s <- [0..VS.length v]]
{-# INLINE inits #-}

-- | /O(n)/ Return all final segments of the given 'ByteString', longest first.
tails :: ByteString -> [ByteString]
tails v | VS.null v = [VS.empty]
        | otherwise = v : tails (VS.unsafeTail v)
{-# INLINE tails #-}

--------------------------------------------------------------------------------
-- ** Breaking into many substrings

-- | /O(n)/ Break a 'ByteString' into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
-- > split 'a'  "aXaXaXa"    == ["","X","X","X",""]
-- > split 'x'  "x"          == ["",""]
--
-- and
--
-- > intercalate [c] . split c == id
-- > split == splitWith . (==)
--
-- As for all splitting functions in this library, this function does
-- not copy the substrings, it just constructs new 'ByteStrings' that
-- are slices of the original.
--
split :: Word8 -> ByteString -> [ByteString]
split w v | l == 0    = []
          | otherwise = go 0
    where
      (fp, l) = unsafeToForeignPtr0 v

      withFP = unsafeInlineIO . withForeignPtr fp

      go !n | q == nullPtr = vec l'    : []
            | otherwise    = vec (i-n) : go (i+1)
          where
            vec = VS.unsafeFromForeignPtr fp n

            q = withFP $ \p -> memchr (p `plusPtr` n) w (fromIntegral l')
            i = withFP $ \p -> return (q `minusPtr` p)

            l' = l - n
{-# INLINE split #-}

-- | /O(n)/ Splits a 'ByteString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
-- > splitWith (=='a') []        == []
--
splitWith :: (Word8 -> Bool) -> ByteString -> [ByteString]
splitWith pred v
    | l == 0    = []
    | otherwise = splitWith0 0 l
    where
      (fp, l) = unsafeToForeignPtr0 v

      splitWith0 off len = unsafeInlineIO $ withForeignPtr fp $ \p ->
        let vec = VS.unsafeFromForeignPtr fp off
            go !idx
                | idx >= len = return [vec idx]
                | otherwise = do
                    let sepIx = off + idx
                    w <- peekElemOff p sepIx
                    if pred w
                      then return (vec idx : splitWith0 (sepIx+1) (len-idx-1))
                      else go (idx+1)
        in go 0
{-# INLINE splitWith #-}


--------------------------------------------------------------------------------
-- * Predicates
--------------------------------------------------------------------------------

-- | /O(n)/ The 'isPrefixOf' function takes two ByteStrings and returns 'True'
-- iff the first is a prefix of the second.
isPrefixOf :: ByteString -> ByteString -> Bool
isPrefixOf v1 v2
    | l1 == 0   = True
    | l2 < l1   = False
    | otherwise = unsafeInlineIO $
                    withForeignPtr fp1 $ \p1 ->
                      withForeignPtr fp2 $ \p2 -> do
                        i <- memcmp p1 p2 (fromIntegral l1)
                        return $! i == 0
    where
      (fp1, l1) = unsafeToForeignPtr0 v1
      (fp2, l2) = unsafeToForeignPtr0 v2
{-# INLINE isPrefixOf #-}

-- | /O(n)/ The 'isSuffixOf' function takes two ByteStrings and returns 'True'
-- iff the first is a suffix of the second.
--
-- The following holds:
--
-- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
--
-- However, the real implemenation uses memcmp to compare the end of the
-- string only, with no reverse required..
isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf v1 v2
    | l1 == 0   = True
    | l2 < l1   = False
    | otherwise = unsafeInlineIO $
                    withForeignPtr fp1 $ \p1 ->
                      withForeignPtr fp2 $ \p2 -> do
        i <- memcmp p1 (p2 `plusPtr` (l2 - l1)) (fromIntegral l1)
        return $! i == 0
    where
      (fp1, l1) = unsafeToForeignPtr0 v1
      (fp2, l2) = unsafeToForeignPtr0 v2
{-# INLINE isSuffixOf #-}

-- | Check whether one string is a substring of another. @isInfixOf
-- p s@ is equivalent to @not (null (findSubstrings p s))@.
isInfixOf :: ByteString -> ByteString -> Bool
isInfixOf v1 v2 = isJust (findSubstring v1 v2)
{-# INLINE isInfixOf #-}

--------------------------------------------------------------------------------
--  ** Search for arbitrary substrings

-- | Break a string on a substring, returning a pair of the part of the
-- string prior to the match, and the rest of the string.
--
-- The following relationships hold:
--
-- > break (== c) l == breakSubstring (singleton c) l
--
-- and:
--
-- > findSubstring s l ==
-- >    if null s then Just 0
-- >              else case breakSubstring s l of
-- >                       (x,y) | null y    -> Nothing
-- >                             | otherwise -> Just (length x)
--
-- For example, to tokenise a string, dropping delimiters:
--
-- > tokenise x y = h : if null t then [] else tokenise x (drop (length x) t)
-- >     where (h,t) = breakSubstring x y
--
-- To skip to the first occurence of a string:
--
-- > snd (breakSubstring x y)
--
-- To take the parts of a string before a delimiter:
--
-- > fst (breakSubstring x y)
--
breakSubstring :: ByteString -- ^ String to search for
               -> ByteString -- ^ String to search in
               -> (ByteString, ByteString)
                             -- ^ Head and tail of string broken at substring
breakSubstring pat src = search 0 src
  where
    search !n !s
        | VS.null s          = (src, VS.empty) -- not found
        | pat `isPrefixOf` s = (VS.take n src, s)
        | otherwise          = search (n+1) (VS.unsafeTail s)
{-# INLINE breakSubstring #-}

-- | Get the first index of a substring in another string,
--   or 'Nothing' if the string is not found.
--   @findSubstring p s@ is equivalent to @listToMaybe (findSubstrings p s)@.
findSubstring :: ByteString -- ^ String to search for.
              -> ByteString -- ^ String to seach in.
              -> Maybe Int
findSubstring f i = listToMaybe (findSubstrings f i)
{-# INLINE findSubstring #-}

{-# DEPRECATED findSubstring "findSubstring is deprecated in favour of breakSubstring." #-}

-- | Find the indexes of all (possibly overlapping) occurances of a
-- substring in a string.
--
findSubstrings :: ByteString -- ^ String to search for.
               -> ByteString -- ^ String to seach in.
               -> [Int]
findSubstrings pat str
    | VS.null pat = [0 .. VS.length str]
    | otherwise   = search 0 str
  where
    search !ix !s
        | VS.null s          = []
        | pat `isPrefixOf` s = ix : ixs
        | otherwise          =      ixs
        where
          ixs = search (ix+1) (VS.unsafeTail s)
{-# INLINE findSubstrings #-}

{-# DEPRECATED findSubstrings "findSubstrings is deprecated in favour of breakSubstring." #-}


--------------------------------------------------------------------------------
-- * Searching ByteStrings
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Searching by equality

-- | /O(n)/ 'elem' is the 'ByteString' membership predicate.
elem :: Word8 -> ByteString -> Bool
elem = VS.elem
{-# INLINE elem #-}

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Word8 -> ByteString -> Bool
notElem = VS.notElem
{-# INLINE notElem #-}

--------------------------------------------------------------------------------
-- ** Searching with a predicate

-- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
find :: (Word8 -> Bool) -> ByteString -> Maybe Word8
find = VS.find
{-# INLINE find #-}

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate. This function is subject to array fusion.
filter :: (Word8 -> Bool) -> ByteString -> ByteString
filter = VS.filter
{-# INLINE filter #-}

-- | /O(n)/ The 'partition' function takes a predicate a ByteString and returns
-- the pair of ByteStrings with elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p bs == (filter p xs, filter (not . p) xs)
--
partition :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
partition = VS.partition
{-# INLINE partition #-}


--------------------------------------------------------------------------------
-- * Indexing ByteStrings
--------------------------------------------------------------------------------

-- | /O(1)/ 'ByteString' index (subscript) operator, starting from 0.
index :: ByteString -> Int -> Word8
index = (VS.!)
{-# INLINE index #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element.
-- This implementation uses memchr(3).
elemIndex :: Word8 -> ByteString -> Maybe Int
elemIndex = VS.elemIndex
{-# INLINE elemIndex #-}

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
-- This implementation uses memchr(3).
elemIndices :: Word8 -> ByteString -> [Int]
elemIndices x v = VS.toList $ VS.elemIndices x v
{-# INLINE elemIndices #-}

-- | /O(n)/ The 'elemIndexEnd' function returns the last index of the
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following
-- holds:
--
-- > elemIndexEnd c xs ==
-- > (-) (length xs - 1) `fmap` elemIndex c (reverse xs)
--
elemIndexEnd :: Word8 -> ByteString -> Maybe Int
elemIndexEnd x v = unsafeInlineIO $ withForeignPtr fp $ \p ->
    let go !i | i < 0     = return Nothing
              | otherwise = do
                  x' <- peekByteOff p i
                  if x == x'
                    then return $ Just i
                    else go (i-1)
    in go (l - 1)
        where
          (fp, l) = unsafeToForeignPtr0 v
{-# INLINE elemIndexEnd #-}

-- | The 'findIndex' function takes a predicate and a 'ByteString' and
-- returns the index of the first element in the ByteString
-- satisfying the predicate.
findIndex :: (Word8 -> Bool) -> ByteString -> Maybe Int
findIndex = VS.findIndex
{-# INLINE findIndex #-}

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Word8 -> Bool) -> ByteString -> [Int]
findIndices pred v = VS.toList $ VS.findIndices pred v
{-# INLINE findIndices #-}

-- | count returns the number of times its argument appears in the ByteString
--
-- > count = length . elemIndices
--
-- But more efficiently than using length on the intermediate list.
count :: Word8 -> ByteString -> Int
count x v = unsafeInlineIO $ withForeignPtr fp $ \p ->
    fmap fromIntegral $ c_count p (fromIntegral l) x
        where
          (fp, l) = unsafeToForeignPtr0 v
{-# INLINE count #-}


--------------------------------------------------------------------------------
-- * Zipping and unzipping ByteStrings
--------------------------------------------------------------------------------

-- | /O(n)/ 'zip' takes two ByteStrings and returns a list of
-- corresponding pairs of bytes. If one input ByteString is short,
-- excess elements of the longer ByteString are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: ByteString -> ByteString -> [(Word8,Word8)]
zip v1 v2
    | VS.null v1 || VS.null v2 = []
    | otherwise = (VS.unsafeHead v1, VS.unsafeHead v2)
                : zip (VS.unsafeTail v1) (VS.unsafeTail v2)
{-# INLINE zip #-}

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two ByteStrings to produce the list of
-- corresponding sums.
zipWith :: (Word8 -> Word8 -> a) -> ByteString -> ByteString -> [a]
zipWith f = go
    where
      go v1 v2
        | VS.null v1 || VS.null v2 = []
        | otherwise = f (VS.unsafeHead v1) (VS.unsafeHead v2)
                    : go (VS.unsafeTail v1) (VS.unsafeTail v2)
{-# INLINE [1] zipWith #-}

{-# RULES
"ByteString specialise zipWith" forall (f :: Word8 -> Word8 -> Word8) p q .
    zipWith f p q = unpack (zipWith' f p q)
  #-}

-- | A specialised version of zipWith for the common case of a
-- simultaneous map over two bytestrings, to build a 3rd. Rewrite rules
-- are used to automatically covert zipWith into zipWith' when a pack is
-- performed on the result of zipWith.
zipWith' :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString -> ByteString
zipWith' f v1 v2 =
    unsafeInlineIO $
      withForeignPtr fp1 $ \p1 ->
         withForeignPtr fp2 $ \p2 ->
           create len $ \p ->
             let go !n
                     | n >= len = return ()
                     | otherwise = do
                         x <- peekByteOff p1 n
                         y <- peekByteOff p2 n
                         pokeByteOff p n (f x y)
                         go (n+1)
             in go 0
    where
      len = min l1 l2

      (fp1, l1) = unsafeToForeignPtr0 v1
      (fp2, l2) = unsafeToForeignPtr0 v2
{-# INLINE zipWith' #-}

-- | /O(n)/ 'unzip' transforms a list of pairs of bytes into a pair of
-- ByteStrings. Note that this performs two 'pack' operations.
unzip :: [(Word8, Word8)] -> (ByteString, ByteString)
unzip ls = ( VS.fromList $ L.map fst ls
           , VS.fromList $ L.map snd ls
           )
{-# INLINE unzip #-}


--------------------------------------------------------------------------------
-- * Ordered ByteStrings
--------------------------------------------------------------------------------

-- | /O(n)/ Sort a ByteString efficiently, using counting sort.
sort :: ByteString -> ByteString
sort v = unsafeCreate l $ \p' -> allocaArray 256 $ \counts -> do

    -- Initialize counts array to all 0s:
    void $ memset (castPtr counts)
                  0
                  (256 * fromIntegral (sizeOf (undefined :: CSize)))

    -- Count occurrences:
    withForeignPtr fp $ \p ->
        let go !i | i == l    = return ()
                  | otherwise = do
                      k <- fromIntegral `fmap` peekElemOff p i
                      x <- peekElemOff counts k
                      pokeElemOff counts k (x + 1)
                      go (i + 1)
        in go 0

    -- Fill result array:
    let go 256 _   = return ()
        go !i !ptr = do
          n <- peekElemOff counts i
          when (n /= 0) $ void $ memset ptr (fromIntegral i) n
          go (i + 1) (ptr `plusPtr` fromIntegral n)
    go 0 p'
  where
    (fp, l) = unsafeToForeignPtr0 v
{-# INLINE sort #-}


--------------------------------------------------------------------------------
-- * Low level conversions
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Copying ByteStrings

-- | /O(n)/ Make a copy of the 'ByteString' with its own storage.
-- This is mainly useful to allow the rest of the data pointed
-- to by the 'ByteString' to be garbage collected, for example
-- if a large string has been read in, and only a small part of it
-- is needed in the rest of the program.
--
copy :: ByteString -> ByteString
copy v = unsafeCreate l $ \p' ->
            withForeignPtr fp $ \p ->
                memcpy p' p (fromIntegral l)
    where
      (fp, l) = unsafeToForeignPtr0 v
{-# INLINE copy #-}

--------------------------------------------------------------------------------
--  ** Packing 'CString's and pointers

-- | /O(n)./ Construct a new @ByteString@ from a @CString@. The
-- resulting @ByteString@ is an immutable copy of the original
-- @CString@, and is managed on the Haskell heap. The original
-- @CString@ must be null terminated.
packCString :: CString -> IO ByteString
packCString cstr = do
    len <- c_strlen cstr
    packCStringLen (cstr, fromIntegral len)
{-# INLINE packCString #-}

-- | /O(n)./ Construct a new @ByteString@ from a @CStringLen@. The
-- resulting @ByteString@ is an immutable copy of the original @CStringLen@.
-- The @ByteString@ is a normal Haskell value and will be managed on the
-- Haskell heap.
packCStringLen :: CStringLen -> IO ByteString
packCStringLen (cstr, len) | len >= 0 = create len $ \p ->
    memcpy p (castPtr cstr) (fromIntegral len)
packCStringLen (_, len) =
    moduleError "packCStringLen" ("negative length: " ++ show len)
{-# INLINE packCStringLen #-}

--------------------------------------------------------------------------------
-- ** Using ByteStrings as 'CString's

-- | /O(n) construction/ Use a @ByteString@ with a function requiring a
-- null-terminated @CString@.  The @CString@ will be freed
-- automatically. This is a memcpy(3).
useAsCString :: ByteString -> (CString -> IO a) -> IO a
useAsCString v action = do
 allocaBytes (l+1) $ \buf ->
    withForeignPtr fp $ \p -> do
      memcpy buf p (fromIntegral l)
      pokeByteOff buf l (0::Word8)
      action (castPtr buf)
    where
      (fp, l) = unsafeToForeignPtr0 v
{-# INLINE useAsCString #-}

-- | /O(n) construction/ Use a @ByteString@ with a function requiring a @CStringLen@.
-- As for @useAsCString@ this function makes a copy of the original @ByteString@.
useAsCStringLen :: ByteString -> (CStringLen -> IO a) -> IO a
useAsCStringLen v f = useAsCString v $ \cstr -> f (cstr, VS.length v)
{-# INLINE useAsCStringLen #-}


--------------------------------------------------------------------------------
--  * I\/O with 'ByteString's
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Standard input and output

-- | Read a line from stdin.
getLine :: IO ByteString
getLine = hGetLine stdin

-- | getContents. Read stdin strictly. Equivalent to hGetContents stdin
-- The 'Handle' is closed after the contents have been read.
--
getContents :: IO ByteString
getContents = hGetContents stdin

-- | Write a ByteString to stdout
putStr :: ByteString -> IO ()
putStr = hPut stdout

-- | Write a ByteString to stdout, appending a newline byte
putStrLn :: ByteString -> IO ()
putStrLn = hPutStrLn stdout

-- | The interact function takes a function of type @ByteString -> ByteString@
-- as its argument. The entire input from the standard input device is passed
-- to this function as its argument, and the resulting string is output on the
-- standard output device.
--
interact :: (ByteString -> ByteString) -> IO ()
interact transformer = putStr . transformer =<< getContents

--------------------------------------------------------------------------------
-- ** Files

-- | Read an entire file strictly into a 'ByteString'.  This is far more
-- efficient than reading the characters into a 'String' and then using
-- 'pack'.  It also may be more efficient than opening the file and
-- reading it using hGet. Files are read using 'binary mode' on Windows,
-- for 'text mode' use the Char8 version of this function.
--
readFile :: FilePath -> IO ByteString
readFile f = withBinaryFile f ReadMode $ \h ->
               hFileSize h >>= hGet h . fromIntegral

-- | Write a 'ByteString' to a file.
writeFile :: FilePath -> ByteString -> IO ()
writeFile f txt = withBinaryFile f WriteMode $ \h -> hPut h txt

-- | Append a 'ByteString' to a file.
appendFile :: FilePath -> ByteString -> IO ()
appendFile f txt = withBinaryFile f AppendMode $ \h -> hPut h txt

--------------------------------------------------------------------------------
-- ** I\/O with Handles

-- | Read a line from a handle

hGetLine :: Handle -> IO ByteString
hGetLine h =
  wantReadableHandle_ "Data.Vector.Storable.ByteString.hGetLine" h $
    \ h_@Handle__{haByteBuffer} -> do
      flushCharReadBuffer h_
      buf <- readIORef haByteBuffer
      if isEmptyBuffer buf
        then fill    h_ buf 0 []
        else haveBuf h_ buf 0 []
 where
  fill h_@Handle__{haByteBuffer, haDevice} buf !len xss = do
    (r, buf') <- Buffered.fillReadBuffer haDevice buf
    if r == 0
       then do writeIORef haByteBuffer buf{ bufR=0, bufL=0 }
               if len > 0
                  then mkBigPS len xss
                  else ioe_EOF
       else haveBuf h_ buf' len xss

  haveBuf h_@Handle__{haByteBuffer}
          buf@Buffer{ bufRaw=raw, bufR=w, bufL=r }
          len xss = do
    off <- findEOL r w raw
    let new_len = len + off - r
    xs <- mkPS raw r off

    -- if eol == True, then off is the offset of the '\n'
    -- otherwise off == w and the buffer is now empty.
    if off /= w
      then do if (w == off + 1)
                then writeIORef haByteBuffer buf{ bufL=0, bufR=0 }
                else writeIORef haByteBuffer buf{ bufL = off + 1 }
              mkBigPS new_len (xs:xss)
      else fill h_ buf{ bufL=0, bufR=0 } new_len (xs:xss)

  -- find the end-of-line character, if there is one
  findEOL r w raw
      | r == w = return w
      | otherwise =  do
          c <- readWord8Buf raw r
          if c == fromIntegral (ord '\n')
            then return r -- NB. not r+1: don't include the '\n'
            else findEOL (r+1) w raw

mkPS :: RawBuffer Word8 -> Int -> Int -> IO ByteString
mkPS buf start end =
 create len $ \p ->
   withRawBuffer buf $ \pbuf -> do
   copyBytes p (pbuf `plusPtr` start) len
 where
   len = end - start

mkBigPS :: Int -> [ByteString] -> IO ByteString
mkBigPS _ [v] = return v
mkBigPS _ vs  = return $! VS.concat (L.reverse vs)

-- | Outputs a 'ByteString' to the specified 'Handle'.
hPut :: Handle -> ByteString -> IO ()
hPut h v
    | l == 0    = return ()
    | otherwise = withForeignPtr fp $ \p -> hPutBuf h p l
    where
      (fp, l) = unsafeToForeignPtr0 v

-- | Similar to 'hPut' except that it will never block. Instead it returns
-- any tail that did not get written. This tail may be 'empty' in the case that
-- the whole string was written, or the whole original string if nothing was
-- written. Partial writes are also possible.
--
-- Note: on Windows and with Haskell implementation other than GHC, this
-- function does not work correctly; it behaves identically to 'hPut'.
--
hPutNonBlocking :: Handle -> ByteString -> IO ByteString
hPutNonBlocking h v = do
  bytesWritten <- withForeignPtr fp $ \p-> hPutBufNonBlocking h p l
  return $! VS.drop bytesWritten v
      where
        (fp, l) = unsafeToForeignPtr0 v

-- | A synonym for @hPut@, for compatibility
hPutStr :: Handle -> ByteString -> IO ()
hPutStr = hPut

-- | Write a ByteString to a handle, appending a newline byte
hPutStrLn :: Handle -> ByteString -> IO ()
hPutStrLn h v
    | VS.length v < 1024 = hPut h (v `snoc` 0x0a)
    | otherwise          = hPut h v >> hPut h (singleton (0x0a)) -- don't copy

{-# DEPRECATED hPutStrLn
    "Use Data.Vector.Storable.ByteString.Char8.hPutStrLn instead. (Functions that rely on ASCII encodings belong in Data.Vector.Storable.ByteString.Char8)"
  #-}
{-# DEPRECATED putStrLn
    "Use Data.Vector.Storable.ByteString.Char8.putStrLn instead. (Functions that rely on ASCII encodings belong in Data.Vector.Storable.ByteString.Char8)"
  #-}

-- | Read a 'ByteString' directly from the specified 'Handle'.  This
-- is far more efficient than reading the characters into a 'String'
-- and then using 'pack'. First argument is the Handle to read from,
-- and the second is the number of bytes to read. It returns the bytes
-- read, up to n, or 'null' if EOF has been reached.
--
-- 'hGet' is implemented in terms of 'hGetBuf'.
--
-- If the handle is a pipe or socket, and the writing end
-- is closed, 'hGet' will behave as if EOF was reached.
--
hGet :: Handle -> Int -> IO ByteString
hGet h i
    | i >  0    = createAndTrim i $ \p -> hGetBuf h p i
    | i == 0    = return VS.empty
    | otherwise = illegalBufferSize h "hGet" i

-- | hGetNonBlocking is identical to 'hGet', except that it will never
-- block waiting for data to become available.  If there is no data
-- available to be read, 'hGetNonBlocking' returns 'null'.
--
hGetNonBlocking :: Handle -> Int -> IO ByteString
hGetNonBlocking h i
    | i >  0    = createAndTrim i $ \p -> hGetBufNonBlocking h p i
    | i == 0    = return VS.empty
    | otherwise = illegalBufferSize h "hGetNonBlocking" i

-- | Like 'hGet', except that a shorter 'ByteString' may be returned
-- if there are not enough bytes immediately available to satisfy the
-- whole request.  'hGetSome' only blocks if there is no data
-- available, and EOF has not yet been reached.
--
hGetSome :: Handle -> Int -> IO ByteString
hGetSome hh i
    | i > 0     = createAndTrim i $ \p -> hGetBufSome hh p i
    | i == 0    = return VS.empty
    | otherwise = illegalBufferSize hh "hGetSome" i

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn sz =
    ioError (mkIOError illegalOperationErrorType msg (Just handle) Nothing)
    --TODO: System.IO uses InvalidArgument here, but it's not exported :-(
    where
      msg = fn ++ ": illegal ByteString size " ++ showsPrec 9 sz []

-- | Read entire handle contents strictly into a 'ByteString'.
--
-- This function reads chunks at a time, doubling the chunksize on each
-- read. The final buffer is then realloced to the appropriate size. For
-- files > half of available memory, this may lead to memory exhaustion.
-- Consider using 'readFile' in this case.
--
-- As with 'hGet', the string representation in the file is assumed to
-- be ISO-8859-1.
--
-- The Handle is closed once the contents have been read,
-- or if an exception is thrown.
--
hGetContents :: Handle -> IO ByteString
hGetContents h = always (hClose h) $ do -- strict, so hClose
    let start_size = 1024
    p <- mallocBytes start_size
    i <- hGetBuf h p start_size
    if i < start_size
        then do p' <- reallocBytes p i
                fp <- newForeignPtr finalizerFree p'
                return $! unsafeFromForeignPtr0 fp i
        else f p start_size
    where
        always = flip finally
        f p s = do
            let s' = 2 * s
            p' <- reallocBytes p s'
            i  <- hGetBuf h (p' `plusPtr` s) s
            if i < s
                then do let i' = s + i
                        p'' <- reallocBytes p' i'
                        fp  <- newForeignPtr finalizerFree p''
                        return $! unsafeFromForeignPtr0 fp i'
                else f p' s'


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

-- | 'findIndexOrEnd' is a variant of findIndex, that returns the length
-- of the string if no element is found, rather than Nothing.
findIndexOrEnd :: (Word8 -> Bool) -> ByteString -> Int
findIndexOrEnd k v = unsafeInlineIO $  withForeignPtr fp $ \p ->
  let end = p `plusPtr` l
      go !ptr | ptr == end = return l
              | otherwise = do
                  w <- peek ptr
                  if k w
                    then return (ptr `minusPtr` p)
                    else go (ptr `plusPtr` 1)
  in go p
    where
      (fp, l) = unsafeToForeignPtr0 v
{-# INLINE findIndexOrEnd #-}

-- | Find from the end of the string using predicate
findFromEndUntil :: (Word8 -> Bool) -> ByteString -> Int
findFromEndUntil pred = go
    where
      go v | VS.null v              = 0
           | pred (VS.unsafeLast v) = l
           | otherwise              = go (VS.unsafeTake (l-1) v)
           where
             l = VS.length v
{-# INLINE findFromEndUntil #-}

moduleError :: String -> String -> a
moduleError fun msg = error $ "Data.Vector.Storable.ByteString." ++
                               fun ++ ':':' ':msg
{-# NOINLINE moduleError #-}
