{-# LANGUAGE CPP
           , NoImplicitPrelude
           , BangPatterns
           , TypeSynonymInstances
           , FlexibleInstances
  #-}

#if defined(__GLASGOW_HASKELL__)
{-# LANGUAGE MagicHash, UnboxedTuples #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Data.Vector.Storable.ByteString.Char8
-- Copyright   : (c) Don Stewart 2006-2008
--               (c) Bas van Dijk 2011
-- License     : BSD-style
--
-- Maintainer  : Bas van Dijk <v.dijk.bas@gmail.com>
-- Stability   : experimental
--
-- Manipulate 'ByteString's using 'Char' operations. All Chars will be
-- truncated to 8 bits. It can be expected that these functions will run
-- at identical speeds to their 'Word8' equivalents in "Data.ByteString".
--
-- More specifically these byte strings are taken to be in the
-- subset of Unicode covered by code points 0-255. This covers
-- Unicode Basic Latin, Latin-1 Supplement and C0+C1 Controls.
--
-- See:
--
--  * <http://www.unicode.org/charts/>
--
--  * <http://www.unicode.org/charts/PDF/U0000.pdf>
--
--  * <http://www.unicode.org/charts/PDF/U0080.pdf>
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.Vector.Storable.ByteString.Char8 as B
--
-- The Char8 interface to bytestrings provides an instance of IsString
-- for the ByteString type, enabling you to use string literals, and
-- have them implicitly packed to ByteStrings. Use -XOverloadedStrings
-- to enable this.
--

module Data.Vector.Storable.ByteString.Char8 (

        -- * The ByteString type
        ByteString,             -- abstract, instances: Eq, Ord, Show, Read, Data, Typeable, Monoid

        -- * Introducing and eliminating ByteStrings
        B.empty,           -- :: ByteString
        singleton,         -- :: Char   -> ByteString
        pack,              -- :: String -> ByteString
        unpack,            -- :: ByteString -> String

        -- * Basic interface
        cons,              -- :: Char -> ByteString -> ByteString
        snoc,              -- :: ByteString -> Char -> ByteString
        B.append,          -- :: ByteString -> ByteString -> ByteString
        head,              -- :: ByteString -> Char
        uncons,            -- :: ByteString -> Maybe (Char, ByteString)
        last,              -- :: ByteString -> Char
        B.tail,            -- :: ByteString -> ByteString
        B.init,            -- :: ByteString -> ByteString
        B.null,            -- :: ByteString -> Bool
        B.length,          -- :: ByteString -> Int

        -- * Transformating ByteStrings
        map,               -- :: (Char -> Char) -> ByteString -> ByteString
        B.reverse,         -- :: ByteString -> ByteString
        intersperse,       -- :: Char -> ByteString -> ByteString
        B.intercalate,     -- :: ByteString -> [ByteString] -> ByteString
        B.transpose,       -- :: [ByteString] -> [ByteString]

        -- * Reducing ByteStrings (folds)
        foldl,             -- :: (a -> Char -> a) -> a -> ByteString -> a
        foldl',            -- :: (a -> Char -> a) -> a -> ByteString -> a
        foldl1,            -- :: (Char -> Char -> Char) -> ByteString -> Char
        foldl1',           -- :: (Char -> Char -> Char) -> ByteString -> Char

        foldr,             -- :: (Char -> a -> a) -> a -> ByteString -> a
        foldr',            -- :: (Char -> a -> a) -> a -> ByteString -> a
        foldr1,            -- :: (Char -> Char -> Char) -> ByteString -> Char
        foldr1',           -- :: (Char -> Char -> Char) -> ByteString -> Char

        -- ** Special folds
        B.concat,          -- :: [ByteString] -> ByteString
        concatMap,         -- :: (Char -> ByteString) -> ByteString -> ByteString
        any,               -- :: (Char -> Bool) -> ByteString -> Bool
        all,               -- :: (Char -> Bool) -> ByteString -> Bool
        maximum,           -- :: ByteString -> Char
        minimum,           -- :: ByteString -> Char

        -- * Building ByteStrings
        -- ** Scans
        scanl,             -- :: (Char -> Char -> Char) -> Char -> ByteString -> ByteString
        scanl1,            -- :: (Char -> Char -> Char) -> ByteString -> ByteString
        scanr,             -- :: (Char -> Char -> Char) -> Char -> ByteString -> ByteString
        scanr1,            -- :: (Char -> Char -> Char) -> ByteString -> ByteString

        -- ** Accumulating maps
        mapAccumL,         -- :: (acc -> Char -> (acc, Char)) -> acc -> ByteString -> (acc, ByteString)
        mapAccumR,         -- :: (acc -> Char -> (acc, Char)) -> acc -> ByteString -> (acc, ByteString)

        -- ** Generating and unfolding ByteStrings
        replicate,         -- :: Int -> Char -> ByteString
        unfoldr,           -- :: (a -> Maybe (Char, a)) -> a -> ByteString
        unfoldrN,          -- :: Int -> (a -> Maybe (Char, a)) -> a -> (ByteString, Maybe a)

        -- * Substrings

        -- ** Breaking strings
        B.take,            -- :: Int -> ByteString -> ByteString
        B.drop,            -- :: Int -> ByteString -> ByteString
        B.splitAt,         -- :: Int -> ByteString -> (ByteString, ByteString)
        takeWhile,         -- :: (Char -> Bool) -> ByteString -> ByteString
        dropWhile,         -- :: (Char -> Bool) -> ByteString -> ByteString
        span,              -- :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
        spanEnd,           -- :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
        break,             -- :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
        breakEnd,          -- :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
        B.group,           -- :: ByteString -> [ByteString]
        groupBy,           -- :: (Char -> Char -> Bool) -> ByteString -> [ByteString]
        B.inits,           -- :: ByteString -> [ByteString]
        B.tails,           -- :: ByteString -> [ByteString]

        -- ** Breaking into many substrings
        split,             -- :: Char -> ByteString -> [ByteString]
        splitWith,         -- :: (Char -> Bool) -> ByteString -> [ByteString]

        -- ** Breaking into lines and words
        lines,             -- :: ByteString -> [ByteString]
        words,             -- :: ByteString -> [ByteString]
        unlines,           -- :: [ByteString] -> ByteString
        unwords,           -- :: ByteString -> [ByteString]

        -- * Predicates
        B.isPrefixOf,      -- :: ByteString -> ByteString -> Bool
        B.isSuffixOf,      -- :: ByteString -> ByteString -> Bool
        B.isInfixOf,       -- :: ByteString -> ByteString -> Bool

        -- ** Search for arbitrary substrings
        B.breakSubstring,  -- :: ByteString -> ByteString -> (ByteString,ByteString)
        B.findSubstring,   -- :: ByteString -> ByteString -> Maybe Int
        B.findSubstrings,  -- :: ByteString -> ByteString -> [Int]

        -- * Searching ByteStrings

        -- ** Searching by equality
        elem,              -- :: Char -> ByteString -> Bool
        notElem,           -- :: Char -> ByteString -> Bool

        -- ** Searching with a predicate
        find,              -- :: (Char -> Bool) -> ByteString -> Maybe Char
        filter,            -- :: (Char -> Bool) -> ByteString -> ByteString

        -- * Indexing ByteStrings
        index,             -- :: ByteString -> Int -> Char
        elemIndex,         -- :: Char -> ByteString -> Maybe Int
        elemIndices,       -- :: Char -> ByteString -> [Int]
        elemIndexEnd,      -- :: Char -> ByteString -> Maybe Int
        findIndex,         -- :: (Char -> Bool) -> ByteString -> Maybe Int
        findIndices,       -- :: (Char -> Bool) -> ByteString -> [Int]
        count,             -- :: Char -> ByteString -> Int

        -- * Zipping and unzipping ByteStrings
        zip,               -- :: ByteString -> ByteString -> [(Char,Char)]
        zipWith,           -- :: (Char -> Char -> c) -> ByteString -> ByteString -> [c]
        unzip,             -- :: [(Char,Char)] -> (ByteString,ByteString)

        -- * Ordered ByteStrings
        B.sort,            -- :: ByteString -> ByteString

        -- * Reading from ByteStrings
        readInt,           -- :: ByteString -> Maybe (Int, ByteString)
        readInteger,       -- :: ByteString -> Maybe (Integer, ByteString)

        -- * Low level CString conversions

        -- ** Copying ByteStrings
        B.copy,            -- :: ByteString -> ByteString

        -- ** Packing CStrings and pointers
        B.packCString,     -- :: CString -> IO ByteString
        B.packCStringLen,  -- :: CStringLen -> IO ByteString

        -- ** Using ByteStrings as CStrings
        B.useAsCString,    -- :: ByteString -> (CString    -> IO a) -> IO a
        B.useAsCStringLen, -- :: ByteString -> (CStringLen -> IO a) -> IO a

        -- * I\/O with ByteStrings

        -- ** Standard input and output
        B.getLine,         -- :: IO ByteString
        B.getContents,     -- :: IO ByteString
        B.putStr,          -- :: ByteString -> IO ()
        putStrLn,          -- :: ByteString -> IO ()
        B.interact,        -- :: (ByteString -> ByteString) -> IO ()

        -- ** Files
        readFile,          -- :: FilePath -> IO ByteString
        writeFile,         -- :: FilePath -> ByteString -> IO ()
        appendFile,        -- :: FilePath -> ByteString -> IO ()

        -- ** I\/O with Handles
        B.hGetLine,        -- :: Handle -> IO ByteString
        B.hGetContents,    -- :: Handle -> IO ByteString
        B.hGet,            -- :: Handle -> Int -> IO ByteString
        B.hGetNonBlocking, -- :: Handle -> Int -> IO ByteString
        B.hPut,            -- :: Handle -> ByteString -> IO ()
        B.hPutNonBlocking, -- :: Handle -> ByteString -> IO ByteString
        B.hPutStr,         -- :: Handle -> ByteString -> IO ()
        hPutStrLn,         -- :: Handle -> ByteString -> IO ()

  ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Monad      ( (>>=), (>>), return )
import Data.Bool          ( Bool(False, True), (&&), (||), not, otherwise )
import Data.Char          ( isSpace )
import Data.Eq            ( (==) )
import Data.Function      ( (.), ($) )
import Data.Functor       ( fmap )
import Data.Int           ( Int )
import Data.Maybe         ( Maybe(Nothing, Just) )
import Data.Ord           ( (<), (<=), (>=) )
import Data.String        ( String, IsString, fromString )
import Data.Tuple         ( fst, snd )
import Foreign.ForeignPtr ( withForeignPtr )
import Foreign.Storable   ( peekElemOff, peekByteOff )
import Prelude            ( Integer, fromIntegral, toInteger, negate
                          , (*), (+), (-), (^), ($!), seq,
                          )
import System.IO          ( IO, FilePath, Handle
                          , IOMode(ReadMode, WriteMode, AppendMode)
                          , withFile, stdout, hFileSize
                          )

import qualified Data.List as L ( length, map, intersperse, filter )

#if defined(__GLASGOW_HASKELL__)
import GHC.Base                 (Char(..),unpackCString#,ord#,int2Word#)
#if __GLASGOW_HASKELL__ >= 611
import GHC.IO                   (stToIO)
#else
import GHC.IOBase               (stToIO)
#endif
import GHC.Prim                 (Addr#,writeWord8OffAddr#,plusAddr#)
import GHC.Ptr                  (Ptr(..))
import GHC.ST                   (ST(..))
#else
import Foreign.Ptr ( plusPtr )
import Data.Char   ( Char )
#endif

-- from vector:
import qualified Data.Vector.Storable as VS

-- from primitive:
import Control.Monad.Primitive ( unsafeInlineIO )

-- from vector-bytestring (this package):
import Data.Vector.Storable.ByteString.Internal ( c2w, w2c, isSpaceWord8 )

import qualified Data.Vector.Storable.ByteString          as B
import qualified Data.Vector.Storable.ByteString.Internal as BI ( unsafeCreate )
import qualified Data.Vector.Storable.ByteString.Unsafe   as BU
    ( unsafeHead, unsafeTail, unsafeTake, unsafeDrop, unsafePackAddress )

import Data.Vector.Storable.ByteString ( ByteString )

import ForeignPtr ( unsafeToForeignPtr0, unsafeFromForeignPtr0 )


------------------------------------------------------------------------

-- | /O(1)/ Convert a 'Char' into a 'ByteString'
singleton :: Char -> ByteString
singleton = B.singleton . c2w
{-# INLINE singleton #-}

instance IsString ByteString where
    fromString = pack
    {-# INLINE fromString #-}

-- | /O(n)/ Convert a 'String' into a 'ByteString'
--
-- For applications with large numbers of string literals, pack can be a
-- bottleneck.
pack :: String -> ByteString
#if !defined(__GLASGOW_HASKELL__)

pack str = B.unsafeCreate (L.length str) $ \p -> go p str
    where go _ []     = return ()
          go p (x:xs) = poke p (c2w x) >> go (p `plusPtr` 1) xs

#else /* hack away */

pack str = BI.unsafeCreate (L.length str) $ \(Ptr p) -> stToIO (go p str)
  where
    go :: Addr# -> [Char] -> ST a ()
    go _ []        = return ()
    go p (C# c:cs) = writeByte p (int2Word# (ord# c)) >> go (p `plusAddr#` 1#) cs

    writeByte p c = ST $ \s# ->
        case writeWord8OffAddr# p 0# c s# of s2# -> (# s2#, () #)
    {-# INLINE writeByte #-}
{-# INLINE [1] pack #-}

{-# RULES
"ByteString pack/packAddress" forall s .
   pack (unpackCString# s) = unsafeInlineIO (BU.unsafePackAddress s)
 #-}

#endif

-- | /O(n)/ Converts a 'ByteString' to a 'String'.
unpack :: ByteString -> [Char]
unpack = L.map w2c . B.unpack
{-# INLINE unpack #-}

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires a memcpy.
cons :: Char -> ByteString -> ByteString
cons = B.cons . c2w
{-# INLINE cons #-}

-- | /O(n)/ Append a Char to the end of a 'ByteString'. Similar to
-- 'cons', this function performs a memcpy.
snoc :: ByteString -> Char -> ByteString
snoc p = B.snoc p . c2w
{-# INLINE snoc #-}

-- | /O(1)/ Extract the head and tail of a ByteString, returning Nothing
-- if it is empty.
uncons :: ByteString -> Maybe (Char, ByteString)
uncons = fmap (\(w,v) -> (w2c w, v)) . B.uncons
{-# INLINE uncons #-}

-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
head :: ByteString -> Char
head = w2c . B.head
{-# INLINE head #-}

-- | /O(1)/ Extract the last element of a packed string, which must be non-empty.
last :: ByteString -> Char
last = w2c . B.last
{-# INLINE last #-}

-- | /O(n)/ 'map' @f xs@ is the ByteString obtained by applying @f@ to each element of @xs@
map :: (Char -> Char) -> ByteString -> ByteString
map f = B.map (c2w . f . w2c)
{-# INLINE map #-}

-- | /O(n)/ The 'intersperse' function takes a Char and a 'ByteString'
-- and \`intersperses\' that Char between the elements of the
-- 'ByteString'.  It is analogous to the intersperse function on Lists.
intersperse :: Char -> ByteString -> ByteString
intersperse = B.intersperse . c2w
{-# INLINE intersperse #-}

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a ByteString, reduces the
-- ByteString using the binary operator, from left to right.
foldl :: (a -> Char -> a) -> a -> ByteString -> a
foldl f = B.foldl (\a c -> f a (w2c c))
{-# INLINE foldl #-}

-- | 'foldl\'' is like foldl, but strict in the accumulator.
foldl' :: (a -> Char -> a) -> a -> ByteString -> a
foldl' f = B.foldl' (\a c -> f a (w2c c))
{-# INLINE foldl' #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a packed string,
-- reduces the packed string using the binary operator, from right to left.
foldr :: (Char -> a -> a) -> a -> ByteString -> a
foldr f = B.foldr (\c a -> f (w2c c) a)
{-# INLINE foldr #-}

-- | 'foldr\'' is a strict variant of foldr
foldr' :: (Char -> a -> a) -> a -> ByteString -> a
foldr' f = B.foldr' (\c a -> f (w2c c) a)
{-# INLINE foldr' #-}

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'ByteStrings'.
foldl1 :: (Char -> Char -> Char) -> ByteString -> Char
foldl1 f ps = w2c (B.foldl1 (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldl1 #-}

-- | A strict version of 'foldl1'
foldl1' :: (Char -> Char -> Char) -> ByteString -> Char
foldl1' f ps = w2c (B.foldl1' (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldl1' #-}

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'ByteString's
foldr1 :: (Char -> Char -> Char) -> ByteString -> Char
foldr1 f ps = w2c (B.foldr1 (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldr1 #-}

-- | A strict variant of foldr1
foldr1' :: (Char -> Char -> Char) -> ByteString -> Char
foldr1' f ps = w2c (B.foldr1' (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldr1' #-}

-- | Map a function over a 'ByteString' and concatenate the results
concatMap :: (Char -> ByteString) -> ByteString -> ByteString
concatMap f = B.concatMap (f . w2c)
{-# INLINE concatMap #-}

-- | Applied to a predicate and a ByteString, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
any :: (Char -> Bool) -> ByteString -> Bool
any f = B.any (f . w2c)
{-# INLINE any #-}

-- | Applied to a predicate and a 'ByteString', 'all' determines if
-- all elements of the 'ByteString' satisfy the predicate.
all :: (Char -> Bool) -> ByteString -> Bool
all f = B.all (f . w2c)
{-# INLINE all #-}

-- | 'maximum' returns the maximum value from a 'ByteString'
maximum :: ByteString -> Char
maximum = w2c . B.maximum
{-# INLINE maximum #-}

-- | 'minimum' returns the minimum value from a 'ByteString'
minimum :: ByteString -> Char
minimum = w2c . B.minimum
{-# INLINE minimum #-}

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new list.
mapAccumL :: (acc -> Char -> (acc, Char))
          -> acc -> ByteString -> (acc, ByteString)
mapAccumL f = B.mapAccumL $ \acc w ->
                case f acc (w2c w) of
                  (acc', c) -> (acc', c2w c)

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from right to left, and returning a
-- final value of this accumulator together with the new ByteString.
mapAccumR :: (acc -> Char -> (acc, Char))
          -> acc -> ByteString -> (acc, ByteString)
mapAccumR f = B.mapAccumR $ \acc w ->
                case f acc (w2c w) of
                  (acc', c) -> (acc', c2w c)

-- | 'scanl' is similar to 'foldl', but returns a list of successive
-- reduced values from the left:
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs.
scanl :: (Char -> Char -> Char) -> Char -> ByteString -> ByteString
scanl f z = B.scanl (\a b -> c2w (f (w2c a) (w2c b))) (c2w z)

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument:
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scanl1 :: (Char -> Char -> Char) -> ByteString -> ByteString
scanl1 f = B.scanl1 (\a b -> c2w (f (w2c a) (w2c b)))

-- | scanr is the right-to-left dual of scanl.
scanr :: (Char -> Char -> Char) -> Char -> ByteString -> ByteString
scanr f z = B.scanr (\a b -> c2w (f (w2c a) (w2c b))) (c2w z)

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
scanr1 :: (Char -> Char -> Char) -> ByteString -> ByteString
scanr1 f = B.scanr1 (\a b -> c2w (f (w2c a) (w2c b)))

-- | /O(n)/ 'replicate' @n x@ is a ByteString of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
--
-- This implemenation uses @memset(3)@
replicate :: Int -> Char -> ByteString
replicate w = B.replicate w . c2w
{-# INLINE replicate #-}

-- | /O(n)/, where /n/ is the length of the result.  The 'unfoldr'
-- function is analogous to the List \'unfoldr\'.  'unfoldr' builds a
-- ByteString from a seed value.  The function takes the element and
-- returns 'Nothing' if it is done producing the ByteString or returns
-- 'Just' @(a,b)@, in which case, @a@ is the next character in the string,
-- and @b@ is the seed value for further production.
--
-- Examples:
--
-- > unfoldr (\x -> if x <= '9' then Just (x, succ x) else Nothing) '0' == "0123456789"
unfoldr :: (a -> Maybe (Char, a)) -> a -> ByteString
unfoldr f x0 = B.unfoldr (fmap k . f) x0
    where k (i, j) = (c2w i, j)

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a ByteString from a seed
-- value.  However, the length of the result is limited by the first
-- argument to 'unfoldrN'.  This function is more efficient than 'unfoldr'
-- when the maximum length of the result is known.
--
-- The following equation relates 'unfoldrN' and 'unfoldr':
--
-- > unfoldrN n f s == take n (unfoldr f s)
unfoldrN :: Int -> (a -> Maybe (Char, a)) -> a -> (ByteString, Maybe a)
unfoldrN n f w = B.unfoldrN n ((k `fmap`) . f) w
    where k (i,j) = (c2w i, j)
{-# INLINE unfoldrN #-}

-- | 'takeWhile', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> ByteString -> ByteString
takeWhile f = B.takeWhile (f . w2c)
{-# INLINE takeWhile #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Char -> Bool) -> ByteString -> ByteString
dropWhile f = B.dropWhile (f . w2c)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] dropWhile #-}
#endif

{-# RULES
"ByteString specialise dropWhile isSpace -> dropSpace"
    dropWhile isSpace = dropSpace
  #-}

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
break f = B.break (f . w2c)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] break #-}
#endif

{-# RULES
"ByteString specialise break (x==)" forall x.
    break ((==) x) = breakChar x
"ByteString specialise break (==x)" forall x.
    break (==x) = breakChar x
  #-}

-- INTERNAL:

-- | 'breakChar' breaks its ByteString argument at the first occurence
-- of the specified char. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
--
-- > break (=='c') "abcd" == breakChar 'c' "abcd"
--
breakChar :: Char -> ByteString -> (ByteString, ByteString)
breakChar c p = case elemIndex c p of
    Nothing -> (p, B.empty)
    Just n  -> (BU.unsafeTake n p, BU.unsafeDrop n p)
{-# INLINE breakChar #-}

-- | 'span' @p xs@ breaks the ByteString into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
span f = B.span (f . w2c)
{-# INLINE span #-}

-- | 'spanEnd' behaves like 'span' but from the end of the 'ByteString'.
-- We have
--
-- > spanEnd (not.isSpace) "x y z" == ("x y ","z")
--
-- and
--
-- > spanEnd (not . isSpace) ps
-- >    ==
-- > let (x,y) = span (not.isSpace) (reverse ps) in (reverse y, reverse x)
--
spanEnd :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd f = B.spanEnd (f . w2c)
{-# INLINE spanEnd #-}

-- | 'breakEnd' behaves like 'break' but from the end of the 'ByteString'
--
-- breakEnd p == spanEnd (not.p)
breakEnd :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
breakEnd f = B.breakEnd (f . w2c)
{-# INLINE breakEnd #-}

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
split :: Char -> ByteString -> [ByteString]
split = B.split . c2w
{-# INLINE split #-}

-- | /O(n)/ Splits a 'ByteString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
--
splitWith :: (Char -> Bool) -> ByteString -> [ByteString]
splitWith f = B.splitWith (f . w2c)
{-# INLINE splitWith #-}
-- the inline makes a big difference here.

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy :: (Char -> Char -> Bool) -> ByteString -> [ByteString]
groupBy k = B.groupBy (\a b -> k (w2c a) (w2c b))

-- | /O(1)/ 'ByteString' index (subscript) operator, starting from 0.
index :: ByteString -> Int -> Char
index = (w2c .) . B.index
{-# INLINE index #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ByteString' which is equal (by memchr) to the
-- query element, or 'Nothing' if there is no such element.
elemIndex :: Char -> ByteString -> Maybe Int
elemIndex = B.elemIndex . c2w
{-# INLINE elemIndex #-}

-- | /O(n)/ The 'elemIndexEnd' function returns the last index of the
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following
-- holds:
--
-- > elemIndexEnd c xs ==
-- > (-) (length xs - 1) `fmap` elemIndex c (reverse xs)
--
elemIndexEnd :: Char -> ByteString -> Maybe Int
elemIndexEnd = B.elemIndexEnd . c2w
{-# INLINE elemIndexEnd #-}

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
elemIndices :: Char -> ByteString -> [Int]
elemIndices = B.elemIndices . c2w
{-# INLINE elemIndices #-}

-- | The 'findIndex' function takes a predicate and a 'ByteString' and
-- returns the index of the first element in the ByteString satisfying the predicate.
findIndex :: (Char -> Bool) -> ByteString -> Maybe Int
findIndex f = B.findIndex (f . w2c)
{-# INLINE findIndex #-}

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Char -> Bool) -> ByteString -> [Int]
findIndices f = B.findIndices (f . w2c)

-- | count returns the number of times its argument appears in the ByteString
--
-- > count = length . elemIndices
--
-- Also
--
-- > count '\n' == length . lines
--
-- But more efficiently than using length on the intermediate list.
count :: Char -> ByteString -> Int
count c = B.count (c2w c)

-- | /O(n)/ 'elem' is the 'ByteString' membership predicate. This
-- implementation uses @memchr(3)@.
elem :: Char -> ByteString -> Bool
elem    c = B.elem (c2w c)
{-# INLINE elem #-}

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Char -> ByteString -> Bool
notElem c = B.notElem (c2w c)
{-# INLINE notElem #-}

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> ByteString -> ByteString
filter f = B.filter (f . w2c)
{-# INLINE filter #-}

-- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
find :: (Char -> Bool) -> ByteString -> Maybe Char
find f ps = w2c `fmap` B.find (f . w2c) ps
{-# INLINE find #-}

-- | /O(n)/ 'zip' takes two ByteStrings and returns a list of
-- corresponding pairs of Chars. If one input ByteString is short,
-- excess elements of the longer ByteString are discarded. This is
-- equivalent to a pair of 'unpack' operations, and so space
-- usage may be large for multi-megabyte ByteStrings
zip :: ByteString -> ByteString -> [(Char,Char)]
zip ps qs
    | B.null ps || B.null qs = []
    | otherwise = (unsafeHead ps, unsafeHead qs)
                : zip (BU.unsafeTail ps) (BU.unsafeTail qs)

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two ByteStrings to produce the list
-- of corresponding sums.
zipWith :: (Char -> Char -> a) -> ByteString -> ByteString -> [a]
zipWith f = B.zipWith ((. w2c) . f . w2c)

-- | 'unzip' transforms a list of pairs of Chars into a pair of
-- ByteStrings. Note that this performs two 'pack' operations.
unzip :: [(Char,Char)] -> (ByteString,ByteString)
unzip ls = (pack (L.map fst ls), pack (L.map snd ls))
{-# INLINE unzip #-}

-- | A variety of 'head' for non-empty ByteStrings. 'unsafeHead' omits
-- the check for the empty case, which is good for performance, but
-- there is an obligation on the programmer to provide a proof that the
-- ByteString is non-empty.
unsafeHead :: ByteString -> Char
unsafeHead  = w2c . BU.unsafeHead
{-# INLINE unsafeHead #-}

-- ---------------------------------------------------------------------
-- Things that depend on the encoding

{-# RULES
"ByteString specialise break -> breakSpace"
    break isSpace = breakSpace
  #-}

-- | 'breakSpace' returns the pair of ByteStrings when the argument is
-- broken at the first whitespace byte. I.e.
--
-- > break isSpace == breakSpace
--
breakSpace :: ByteString -> (ByteString,ByteString)
breakSpace v = unsafeInlineIO $ withForeignPtr fp $ \p ->
    let go !i
            | i >= l    = return (vec l, B.empty)
            | otherwise = do
                w <- peekByteOff p i
                if (not . isSpaceWord8) w
                  then go (i+1)
                  else return $!
                       if i == 0
                       then (B.empty, vec l)
                       else (vec i, VS.unsafeFromForeignPtr fp i (l-i))
    in go 0
        where
          (fp, l) = unsafeToForeignPtr0 v
          vec = unsafeFromForeignPtr0 fp
{-# INLINE breakSpace #-}

-- | 'dropSpace' efficiently returns the 'ByteString' argument with
-- white space Chars removed from the front. It is more efficient than
-- calling dropWhile for removing whitespace. I.e.
--
-- > dropWhile isSpace == dropSpace
--
dropSpace :: ByteString -> ByteString
dropSpace v = unsafeInlineIO $ withForeignPtr fp $ \p ->
    let go !i
            | i >= l    = return B.empty
            | otherwise = do
                w <- peekElemOff p i
                if isSpaceWord8 w
                  then go (i+1)
                  else return $ VS.unsafeFromForeignPtr fp i (l-i)
    in go 0
        where
          (fp, l) = unsafeToForeignPtr0 v
{-# INLINE dropSpace #-}

-- | 'lines' breaks a ByteString up into a list of ByteStrings at
-- newline Chars. The resulting strings do not contain newlines.
lines :: ByteString -> [ByteString]
lines ps
    | B.null ps = []
    | otherwise = case search ps of
             Nothing -> [ps]
             Just n  -> BU.unsafeTake n ps : lines (BU.unsafeDrop (n+1) ps)
    where search = elemIndex '\n'

-- | 'unlines' is an inverse operation to 'lines'.  It joins lines,
-- after appending a terminating newline to each.
unlines :: [ByteString] -> ByteString
unlines [] = B.empty
unlines ss = B.concat (L.intersperse nl ss) `B.append` nl -- half as much space
    where nl = singleton '\n'

-- | 'words' breaks a ByteString up into a list of words, which
-- were delimited by Chars representing white space.
words :: ByteString -> [ByteString]
words = L.filter (not . B.null) . B.splitWith isSpaceWord8
{-# INLINE words #-}

-- | The 'unwords' function is analogous to the 'unlines' function, on words.
unwords :: [ByteString] -> ByteString
unwords = B.intercalate (singleton ' ')
{-# INLINE unwords #-}

-- ---------------------------------------------------------------------
-- Reading from ByteStrings

-- | readInt reads an Int from the beginning of the ByteString.  If there is no
-- integer at the beginning of the string, it returns Nothing, otherwise
-- it just returns the int read, and the rest of the string.
readInt :: ByteString -> Maybe (Int, ByteString)
readInt as
    | B.null as = Nothing
    | otherwise =
        case unsafeHead as of
            '-' -> loop True  0 0 (BU.unsafeTail as)
            '+' -> loop False 0 0 (BU.unsafeTail as)
            _   -> loop False 0 0 as

    where loop :: Bool -> Int -> Int -> ByteString -> Maybe (Int, ByteString)
          loop !neg !i !n !ps
              | B.null ps = end neg i n ps
              | otherwise =
                  case BU.unsafeHead ps of
                    w | w >= 0x30
                     && w <= 0x39 -> loop neg (i+1)
                                          (n * 10 + (fromIntegral w - 0x30))
                                          (BU.unsafeTail ps)
                      | otherwise -> end neg i n ps

          end _    0 _ _  = Nothing
          end True _ n ps = Just (negate n, ps)
          end _    _ n ps = Just (n, ps)

-- | readInteger reads an Integer from the beginning of the ByteString.  If
-- there is no integer at the beginning of the string, it returns Nothing,
-- otherwise it just returns the int read, and the rest of the string.
readInteger :: ByteString -> Maybe (Integer, ByteString)
readInteger as
    | B.null as = Nothing
    | otherwise =
        case unsafeHead as of
            '-' -> first (BU.unsafeTail as) >>= \(n, bs) -> return (-n, bs)
            '+' -> first (BU.unsafeTail as)
            _   -> first as

    where first ps | B.null ps = Nothing
                   | otherwise =
                       case BU.unsafeHead ps of
                        w | w >= 0x30 && w <= 0x39 -> Just $
                            loop 1 (fromIntegral w - 0x30) [] (BU.unsafeTail ps)
                          | otherwise              -> Nothing

          loop :: Int -> Int -> [Integer]
               -> ByteString -> (Integer, ByteString)
          loop !d !acc !ns !ps
              | B.null ps = combine d acc ns B.empty
              | otherwise =
                  case BU.unsafeHead ps of
                   w | w >= 0x30 && w <= 0x39 ->
                       if d == 9 then loop 1 (fromIntegral w - 0x30)
                                           (toInteger acc : ns)
                                           (BU.unsafeTail ps)
                                 else loop (d+1)
                                           (10*acc + (fromIntegral w - 0x30))
                                           ns (BU.unsafeTail ps)
                     | otherwise -> combine d acc ns ps

          combine _ acc [] ps = (toInteger acc, ps)
          combine d acc ns ps =
              ((10^d * combine1 1000000000 ns + toInteger acc), ps)

          combine1 _ [n] = n
          combine1 b ns  = combine1 (b*b) $ combine2 b ns

          combine2 b (n:m:ns) = let t = m*b + n in t `seq` (t : combine2 b ns)
          combine2 _ ns       = ns

------------------------------------------------------------------------
-- For non-binary text processing:

-- | Read an entire file strictly into a 'ByteString'.  This is far more
-- efficient than reading the characters into a 'String' and then using
-- 'pack'.  It also may be more efficient than opening the file and
-- reading it using hGet.
readFile :: FilePath -> IO ByteString
readFile f = withFile f ReadMode $ \h ->
               hFileSize h >>= B.hGet h . fromIntegral

-- | Write a 'ByteString' to a file.
writeFile :: FilePath -> ByteString -> IO ()
writeFile f txt = withFile f WriteMode $ \h -> B.hPut h txt

-- | Append a 'ByteString' to a file.
appendFile :: FilePath -> ByteString -> IO ()
appendFile f txt = withFile f AppendMode $ \h -> B.hPut h txt

-- | Write a ByteString to a handle, appending a newline byte
hPutStrLn :: Handle -> ByteString -> IO ()
hPutStrLn h ps
    | B.length ps < 1024 = B.hPut h (ps `B.snoc` 0x0a)
    | otherwise          = B.hPut h ps >> B.hPut h (B.singleton (0x0a)) -- don't copy

-- | Write a ByteString to stdout, appending a newline byte
putStrLn :: ByteString -> IO ()
putStrLn = hPutStrLn stdout
