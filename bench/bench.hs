{-# LANGUAGE CPP, BangPatterns #-}

-- Disable warnings for the orphaned NFData instances for legacy ByteStrings:
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Disable warnings for the deprecated findSubstring and findSubstrings:
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Exception ( evaluate )
import Data.List         ( sortBy )
import Data.Word         ( Word8 )
import Data.Char         ( isUpper, isAlpha, toLower )
import Data.Monoid       ( mappend )
import System.IO         ( withFile, IOMode(ReadMode) )
import Foreign.C.String  ( withCString, withCStringLen )

import qualified Data.List as List ( replicate )

-- from deepseq:
import Control.DeepSeq ( NFData, rnf, deepseq )

-- from criterion:
import Criterion.Main ( Benchmarkable, Benchmark
                      , defaultMain, bgroup, bench, bcompare, nf
                      )

-- from vector-bytestring:
import qualified Data.Vector.Storable.ByteString             as VSB
import qualified Data.Vector.Storable.ByteString.Lazy        as VSBL
import qualified Data.Vector.Storable.ByteString.Char8       as VSB8
import qualified Data.Vector.Storable.ByteString.Lazy.Char8  as VSBL8
import qualified Data.Vector.Storable.ByteString.Unsafe      as VSBU
import qualified Data.Vector.Storable.ByteString.Internal    as VSBI
import           Data.Vector.Storable.ByteString.Internal ( c2w, w2c )

-- from bytestring:
import qualified Data.ByteString                             as B
import qualified Data.ByteString.Lazy                        as BL
import qualified Data.ByteString.Char8                       as B8
import qualified Data.ByteString.Lazy.Char8                  as BL8
import qualified Data.ByteString.Unsafe                      as BU
import qualified Data.ByteString.Internal                    as BI

#if !MIN_VERSION_bytestring(0,10,0)
import qualified Data.ByteString.Lazy.Internal               as BLI

instance NFData BL.ByteString where
    rnf BLI.Empty = ()
    rnf (BLI.Chunk _ cs) = rnf cs

instance NFData B.ByteString
#endif

#if !MIN_VERSION_vector(0,10,0)
instance NFData VSB.ByteString
#endif


--------------------------------------------------------------------------------
-- Handy CPP macros
--------------------------------------------------------------------------------

#define BOO8(name, vb, b, vb8, b8, vbl, bl, vbl8, bl8) \
        (boo "name" (nf   VSB.name vb)   \
                    (nf     B.name b)    \
                    (nf  VSB8.name vb8)  \
                    (nf    B8.name b8)   \
                    (nf  VSBL.name vbl)  \
                    (nf    BL.name bl)   \
                    (nf VSBL8.name vbl8) \
                    (nf   BL8.name bl8))

#define BOO4(name, vb, b, vbl, bl) BOO8(name, vb, b, vb, b,  vbl, bl, vbl, bl)
#define BOO2(name, a, a8)          BOO8(name, a,  a, a8, a8, a,   a,  a8,  a8)

#define BOOA8(name,  vb,  vb_,   b,  b_,   vb8,  vb8_,   b8,  b8_   \
                  ,  vbl, vbl_,  bl, bl_,  vbl8, vbl8_,  bl8, bl8_) \
        (boo "name" (nf   (VSB.name vb)   vb_)   \
                    (nf     (B.name b)    b_)    \
                    (nf  (VSB8.name vb8)  vb8_)  \
                    (nf    (B8.name b8)   b8_)   \
                    (nf  (VSBL.name vbl)  vbl_)  \
                    (nf    (BL.name bl)   bl_)   \
                    (nf (VSBL8.name vbl8) vbl8_) \
                    (nf   (BL8.name bl8)  bl8_)) \

#define BOOA(name, a, a8, vb, b, vbl, bl) \
        BOOA8(name, a, vb,   a, b,   a8, vb,   a8, b \
                  , a, vbl,  a, bl,  a8, vbl,  a8, bl)

#define BOOB(name, a, a8, vb, b, vbl, bl) \
        BOOA8(name, vb,  a,  b,  a,  vb,  a8,  b,  a8 \
                  , vbl, a,  bl, a,  vbl, a8,  bl, a8)

#define BLOO(name,  vb, vb_,  b, b_,  vbl, vbl_,  bl, bl_) \
        (blo "name" (nf   (VSB.name vb)  vb_)  \
                    (nf     (B.name b)   b_)   \
                    (nf  (VSBL.name vbl) vbl_) \
                    (nf    (BL.name bl)  bl_)) \

#define BLOSL(name, s, l, vb, b, vbl, bl) \
        BLOO(name,  s, vb,  s, b,  l, vbl,  l, bl)

#define BLAA(name, a, a8, vb, b)           \
        (bla "name" (nf  (VSB.name a)  vb) \
                    (nf    (B.name a)  b)  \
                    (nf (VSB8.name a8) vb) \
                    (nf   (B8.name a8) b))

#define BLO(name,   vb, b, vbl, bl)    \
        (blo "name" (nf  VSB.name vb)  \
                    (nf    B.name b)   \
                    (nf VSBL.name vbl) \
                    (nf   BL.name bl))


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

deepEvaluate :: NFData a => a -> IO ()
deepEvaluate = evaluate . rnf

main :: IO ()
main = do
  let dict = "tests/data"

  vb  <- VSB.readFile dict
  b   <-   B.readFile dict
  vbl <-VSBL.readFile dict
  bl  <-  BL.readFile dict

  deepseq (vb, b, vbl, bl) $
    defaultMain $
    [
    ----------------------------------------------------------------------------
    -- * Introducing and eliminating 'ByteString's
    ----------------------------------------------------------------------------

      blo "empty" (nf (const  VSB.empty) ())
                  (nf (const    B.empty) ())
                  (nf (const VSBL.empty) ())
                  (nf (const   BL.empty) ())

    , let !z  = 0
          !z8 = w2c z
      in BOO2(singleton, z, z8)

    , let xs =  B.unpack b
          cs = B8.unpack b
      in (xs, cs) `deepseq`
         BOO2(pack, xs, cs)

    , BOO4(unpack, vb, b, vbl, bl)

    , let f  = (+1)
          f8 = f . c2w
          consume = foldr (\_ z -> z) (0 :: Word8)
      in boo "unpack_list_fuse" (nf (consume . map f  .   VSB.unpack) vb)
                                (nf (consume . map f  .     B.unpack) b)
                                (nf (consume . map f8 .  VSB8.unpack) vb)
                                (nf (consume . map f8 .    B8.unpack) b)
                                (nf (consume . map f  .  VSBL.unpack) vbl)
                                (nf (consume . map f  .    BL.unpack) bl)
                                (nf (consume . map f8 . VSBL8.unpack) vbl)
                                (nf (consume . map f8 .   BL8.unpack) bl)


    ----------------------------------------------------------------------------
    --  * Basic interface
    ----------------------------------------------------------------------------

    , let !z  = 0
          !z8 = w2c z
      in BOOA(cons, z, z8, vb, b, vbl, bl)

    , let !z  = 0
          !z8 = w2c z
      in BOOB(snoc, z, z8, vb, b, vbl, bl)

    , BLOO(append,   vb, vb,   b, b,   vbl, vbl,   bl, bl)

    , BOO4(head,   vb, b, vbl, bl)
    , BOO4(uncons, vb, b, vbl, bl)
#if MIN_VERSION_bytestring(0,10,2)
    , BOO4(unsnoc, vb, b, vbl, bl)
#endif
    , BOO4(last,   vb, b, vbl, bl)

    , BLO(tail,   vb, b, vbl, bl)
    , BLO(init,   vb, b, vbl, bl)
    , BLO(null,   vb, b, vbl, bl)
    , BLO(length, vb, b, vbl, bl)


    ----------------------------------------------------------------------------
    -- * Transforming ByteStrings
    ----------------------------------------------------------------------------

    , let f  = (+1)
          f8 = w2c . f . c2w
      in BOOA(map, f, f8, vb, b, vbl, bl)

    , BLO(reverse, vb, b, vbl, bl)

    , let !z  = 0
          !z8 = w2c z
      in BOOA(intersperse, z, z8,  vb, b, vbl, bl)

    , let n      = 100
          vbsN   = List.replicate n vb
          bsN    = List.replicate n b
          vblsN  = List.replicate n vbl
          blsN   = List.replicate n bl
      in (vbsN, bsN, vblsN, blsN) `deepseq`
         BLOO(intercalate,   vb, vbsN,   b, bsN,   vbl, vblsN,   bl, blsN)

      -- TODO: See if the RULE
      -- "ByteString specialise intercalate c -> intercalateByte" fires:
    , let n      = 100
          vbsN   = List.replicate n vb
          bsN    = List.replicate n b
          !z     = 0
      in (vbsN, bsN) `deepseq`
         bli "intercalate_singleton"
                 (nf (VSB.intercalate (VSB.singleton z)) vbsN)
                 (nf   (B.intercalate (  B.singleton z))  bsN)

    , let m      = 5
          vbsM   = List.replicate m vb
          bsM    = List.replicate m b
          vblsM  = List.replicate m vbl
          blsM   = List.replicate m bl
      in (vbsM, bsM, vblsM, blsM) `deepseq`
         BLO(transpose, vbsM, bsM, vblsM, blsM)


    ----------------------------------------------------------------------------
    -- * Reducing 'ByteString's (folds)
    ----------------------------------------------------------------------------

    , let
          f  y x = y + x
          f8 y x = w2c $ f (c2w y) (c2w x)
          !z     = 0
          !z8    = w2c z

          -- TODO:
          -- Enabling these arguments instead of the former causes GHC to loop!!!
          -- See ticket: http://hackage.haskell.org/trac/ghc/ticket/5550

          -- f  xs x = x:xs
          -- f8 xs x = x:xs
          -- z  = []
          -- z8 = []

      in BOOA(foldl, f z, f8 z8, vb, b, vbl, bl)

    , let f  y x = x + y
          f8 y x = w2c $ f (c2w y) (c2w x)
          !z     = 0
          !z8    = w2c z
      in boo "foldl_strict" (nf   (VSB.foldl' f  z)  vb)
                            (nf     (B.foldl' f  z)  b)
                            (nf  (VSB8.foldl' f8 z8) vb)
                            (nf    (B8.foldl' f8 z8) b)
                            (nf  (VSBL.foldl' f  z)  vbl)
                            (nf    (BL.foldl' f  z)  bl)
                            (nf (VSBL8.foldl' f8 z8) vbl)
                            (nf   (BL8.foldl' f8 z8) bl)

    , let f  y x = x + y
          f8 y x = w2c $ f (c2w y) (c2w x)
          n      = 100000 -- TODO: Increasing this causes stack overflows in:
                          -- foldl1/strict/Word8/vector !
                          -- But why not in:
                          -- foldl1/strict/Word8/bytestring ?
          n64    = fromIntegral n
          vb2    =  VSB.take n   vb
          b2     =    B.take n   b
          vbl2   = VSBL.take n64 vbl
          bl2    =   BL.take n64 bl
      in (vb2, b2, vbl2, bl2) `deepseq`
         BOOA(foldl1, f, f8, vb2, b2, vbl2, bl2)

    , let f  y x = x + y
          f8 y x = w2c $ f (c2w y) (c2w x)
      in boo "foldl1_strict" (nf   (VSB.foldl1' f)  vb)
                             (nf     (B.foldl1' f)  b)
                             (nf  (VSB8.foldl1' f8) vb)
                             (nf    (B8.foldl1' f8) b)
                             (nf  (VSBL.foldl1' f)  vbl)
                             (nf    (BL.foldl1' f)  bl)
                             (nf (VSBL8.foldl1' f8) vbl)
                             (nf   (BL8.foldl1' f8) bl)

    , let f  = (:)
          f8 = (:)
          z  = []
          z8 = []
      in BOOA(foldr, f  z, f8 z8, vb, b, vbl, bl)

    , let f  y x = x + y
          f8 y x = w2c $ f (c2w y) (c2w x)
          !z     = 0
          !z8    = w2c z
      in bla "foldr_strict" (nf   (VSB.foldr' f  z)  vb)
                            (nf     (B.foldr' f  z)  b)
                            (nf  (VSB8.foldr' f8 z8) vb)
                            (nf    (B8.foldr' f8 z8) b)

    , let f  y x = x + y
          f8 y x = w2c $ f (c2w y) (c2w x)
          n      = 100000 -- TODO: Increasing this causes stack overflows in:
                          -- foldr1/strict/Char8/vector !
                          -- But why not in:
                          -- foldr1/strict/Char8/bytestring ?
          n64    = fromIntegral n
          vb2    =  VSB.take n   vb
          b2     =    B.take n   b
          vbl2   = VSBL.take n64 vbl
          bl2    =   BL.take n64 bl
      in (vb2, b2, vbl2, bl2) `deepseq`
         BOOA(foldr1, f, f8, vb2, b2, vbl2, bl2)

    , let f  y x = x + y
          f8 y x = w2c $ f (c2w y) (c2w x)
      in bla "foldr1_strict" (nf   (VSB.foldr1' f)  vb)
                             (nf     (B.foldr1' f)  b)
                             (nf  (VSB8.foldr1' f8) vb)
                             (nf    (B8.foldr1' f8) b)

    ----------------------------------------------------------------------------
    -- ** Special folds

    , let m      = 5
          vbsM   = List.replicate m vb
          bsM    = List.replicate m b
          vblsM  = List.replicate m vbl
          blsM   = List.replicate m bl
      in (vbsM, bsM, vblsM, blsM) `deepseq`
         BLO(concat, vbsM, bsM, vblsM, blsM)

    , let !r   = 5
          !r64 = fromIntegral r
      in boo "concatMap" (nf   (VSB.concatMap (  VSB.replicate r))   vb)
                         (nf     (B.concatMap (    B.replicate r))   b)
                         (nf  (VSB8.concatMap ( VSB8.replicate r))   vb)
                         (nf    (B8.concatMap (   B8.replicate r))   b)
                         (nf  (VSBL.concatMap ( VSBL.replicate r64)) vbl)
                         (nf    (BL.concatMap (   BL.replicate r64)) bl)
                         (nf (VSBL8.concatMap (VSBL8.replicate r64)) vbl)
                         (nf   (BL8.concatMap (  BL8.replicate r64)) bl)

    , let p  = (== 255)
          p8 = p . c2w
      in BOOA(any, p, p8, vb, b, vbl, bl)

    , let p  = (<= 255)
          p8 = p . c2w
      in BOOA(all, p, p8, vb, b, vbl, bl)

    , BOO4(maximum, vb, b, vbl, bl)
    , BOO4(minimum, vb, b, vbl, bl)


    ----------------------------------------------------------------------------
    -- * Building ByteStrings
    ----------------------------------------------------------------------------

    ----------------------------------------------------------------------------
    -- ** Scans

    , let f  x y = x + y
          f8 x y = w2c $ f (c2w x) (c2w y)
          !z     = 1
          !z8    = w2c z
          n      = 1000 -- TODO: Increasing this causes stack space overflows in:
                        -- scanl/lazy/Word8/vector
                        -- scanl/lazy/Word8/bytestring
                        -- scanl/lazy/Char8/vector
                        -- scanl/lazy/Char8/bytestring
          n64    = fromIntegral n
          vb2    =  VSB.take n   vb
          b2     =    B.take n   b
          vbl2   = VSBL.take n64 vbl
          bl2    =   BL.take n64 bl
      in (vb2, b2, vbl2, bl2) `deepseq`
         BOOA(scanl, f z, f8 z8, vb2, b2, vbl2, bl2)

    , let f  x y = x + y
          f8 x y = w2c $ f (c2w x) (c2w y)
      in BLAA(scanl1, f, f8, vb, b)

    , let f  x y = x + y
          f8 x y = w2c $ f (c2w x) (c2w y)
          !z     = 1
          !z8    = w2c z
      in BLAA(scanr, f z, f8 z8, vb, b)

    , let f  x y = x + y
          f8 x y = w2c $ f (c2w x) (c2w y)
      in BLAA(scanr1, f, f8, vb, b)

    ----------------------------------------------------------------------------
    -- ** Accumulating maps

    , let f  acc x = (x:acc, x * x + x)
          f8 acc c = (c:acc, w2c $ c2w c * c2w c + c2w c)
      in BOOA(mapAccumL, f [], f8 [], vb, b, vbl, bl)

    , let f  acc x = (x:acc, x * x + x)
          f8 acc c = (c:acc, w2c $ c2w c * c2w c + c2w c)
      in BOOA(mapAccumR, f [], f8 [], vb, b, vbl, bl)

    ----------------------------------------------------------------------------
    -- ** Generating and unfolding ByteStrings

    , let !o   = 1000000
          !o64 = fromIntegral o
          !z   = 0
          !z8  = w2c z
      in BOOB(replicate, z, z8, o, o, o64, o64)
    ]
    ++
    ( let f :: Int -> Maybe (Word8, Int)
          f 1000000 = Nothing
          f i       = Just (fromIntegral i, i+1)

          f8 :: Int -> Maybe (Char, Int)
          f8 1000000 = Nothing
          f8 i       = Just (w2c $ fromIntegral i, i+1)

      in [ BOOA(unfoldr, f, f8, 0, 0, 0, 0)
         , let !k = 1000000
           in BLAA(unfoldrN, k f, k f8, 0, 0)
         ]
    ) ++


    ----------------------------------------------------------------------------
    -- * Substrings
    ----------------------------------------------------------------------------

    ----------------------------------------------------------------------------
    -- ** Breaking strings

    [ let !t   = 260000
          !t64 = fromIntegral t
      in BLOSL(take, t, t64, vb, b, vbl, bl)

    , let !d   = 10000
          !d64 = fromIntegral d
      in BLOSL(drop, d, d64, vb, b, vbl, bl)

    , let !s   = 260000 `div` 2
          !s64 = fromIntegral s
      in BLOSL(splitAt, s, s64, vb, b, vbl, bl)

    , let p  = (<= 255) -- take everything
          p8 = p . c2w
      in BOOA(takeWhile, p, p8, vb, b, vbl, bl)
      -- TODO: takeWhile/strict/Char8/vector is suspiciously fast!

    , let p  = (<= 255) -- drop everything
          p8 = p . c2w
      in BOOA(dropWhile, p, p8, vb, b, vbl, bl)
      -- TODO: dropWhile/strict/Char8/vector is suspiciously fast!

    , let p  = (<= 255) -- span till end
          p8 = p . c2w
      in BOOA(span, p, p8, vb, b, vbl, bl)

      -- See if the RULE: "ByteString specialise span (==x)" fires:
    , let !n      = 500000
          !n64    = fromIntegral n
          !x      = 1
          !y      = 2
          vbSpan  =  VSB.replicate n   x `mappend`  VSB.replicate n   y
          bSpan   =    B.replicate n   x `mappend`    B.replicate n   y
          vblSpan = VSBL.replicate n64 x `mappend` VSBL.replicate n64 y
          blSpan  =   BL.replicate n64 x `mappend`   BL.replicate n64 y
          p       = (==x)
          p8      = p . c2w
          {-# INLINE p  #-}
          {-# INLINE p8 #-}
      in (vbSpan, bSpan, vblSpan, blSpan) `deepseq`
         boo "span_eq" (nf   (VSB.span p)  vbSpan) -- TODO: Does the RULE fire?
                       (nf     (B.span p)  bSpan)
                       (nf  (VSB8.span p8) vbSpan)
                       (nf    (B8.span p8) bSpan)
                       (nf  (VSBL.span p)  vblSpan)
                       (nf    (BL.span p)  blSpan)
                       (nf (VSBL8.span p8) vblSpan)
                       (nf   (BL8.span p8) blSpan)

    , let p  = (<= 255)
          p8 = p . c2w
      in BLAA(spanEnd, p, p8, vb, b)

    , let p  = (>= 255)
          p8 = p . c2w
      in BOOA(break, p, p8, vb, b, vbl, bl)

      -- See if the RULE: "ByteString specialise break (==x)" fires:
    , let !n      = 500000
          !n64    = fromIntegral n
          !x      = 1
          !y      = 2
          vbSpan  =  VSB.replicate n   x `mappend`  VSB.replicate n   y
          bSpan   =    B.replicate n   x `mappend`    B.replicate n   y
          vblSpan = VSBL.replicate n64 x `mappend` VSBL.replicate n64 y
          blSpan  =   BL.replicate n64 x `mappend`   BL.replicate n64 y
          p       = (==y)
          p8      = p . c2w
          {-# INLINE p  #-}
          {-# INLINE p8 #-}
      in (vbSpan, bSpan, vblSpan, blSpan) `deepseq`
         boo "break_eq" (nf   (VSB.break p)  vbSpan) -- TODO: Does the RULE fire?
                        (nf     (B.break p)  bSpan)
                        (nf  (VSB8.break p8) vbSpan)
                        (nf    (B8.break p8) bSpan)
                        (nf  (VSBL.break p)  vblSpan)
                        (nf    (BL.break p)  blSpan)
                        (nf (VSBL8.break p8) vblSpan)
                        (nf   (BL8.break p8) blSpan)

    , let p  = (>= 255)
          p8 = p . c2w
      in BLAA(breakEnd, p, p8, vb, b)

    , BLO(group, vb, b, vbl, bl)

    , let r  x y = x < y
          r8 x y = r (c2w x) (c2w y)
      in BOOA(groupBy, r, r8, vb, b, vbl, bl)

    , BLO(inits, vb, b, vbl, bl)
    , BLO(tails, vb, b, vbl, bl)

    ----------------------------------------------------------------------------
    -- ** Breaking into many substrings

    , let !nlWord = c2w nlChar
          !nlChar = '\n'
      in BOOA(split, nlWord, nlChar, vb, b, vbl, bl)

    , let !w = c2w 'k'
          p  = (>=w)
          p8 = p . c2w
      in BOOA(splitWith, p, p8, vb, b, vbl, bl)

      -- See if the RULE: "ByteString specialise splitWith (==x)" fires:
    , let !nlWord = c2w nlChar
          !nlChar = '\n'
          p       = (==nlWord)
          p8      = p . c2w
          {-# INLINE p  #-}
          {-# INLINE p8 #-}
      in boo "splitWith_eq"
             (nf   (VSB.splitWith p)  vb) -- TODO: Should be as fast as split
                                          --       but isn't !!!
             (nf     (B.splitWith p)  b)
             (nf  (VSB8.splitWith p8) vb)
             (nf    (B8.splitWith p8) b)
             (nf  (VSBL.splitWith p)  vbl)
             (nf    (BL.splitWith p)  bl)
             (nf (VSBL8.splitWith p8) vbl)
             (nf   (BL8.splitWith p8) bl)


    ----------------------------------------------------------------------------
    -- * Predicates
    ----------------------------------------------------------------------------

    , let p    = 1
          p64  = fromIntegral p
          vbp  =  VSB.take p   vb
          bp   =    B.take p   b
          vblp = VSBL.take p64 vbl
          blp  =   BL.take p64 bl
      in (vbp, bp, vblp, blp) `deepseq`
         BLOO(isPrefixOf,   vbp, vb,   bp, b,   vblp, vbl,   blp, bl)

    , let p    = VSB.length vb - 1
          vbp  = VSB.drop p vb
          bp   =   B.drop p b
      in (vbp, bp) `deepseq`
         bli "isSuffixOf" (nf (VSB.isSuffixOf vbp) vb)
                          (nf   (B.isSuffixOf bp)  b)

    , let p   = 100
          m   = VSB.length vb `div` 2
          n   = m - p
          o   = 2 * p
          vbp = VSB.take o (VSB.drop n vb)
          bp  =   B.take o   (B.drop n b)
      in (vbp, bp) `deepseq`
         bli "isInfixOf" (nf (VSB.isInfixOf vbp) vb)
                         (nf   (B.isInfixOf bp)  b)

    ----------------------------------------------------------------------------
    --  ** Search for arbitrary substrings

    , let p   = 100
          m   = VSB.length vb `div` 2
          n   = m - p
          o   = 2 * p
          vbp = VSB.take o (VSB.drop n vb)
          bp  =   B.take o   (B.drop n b)
      in (vbp, bp) `deepseq`
         bli "breakSubstring" (nf (VSB.breakSubstring vbp) vb)
                              (nf   (B.breakSubstring bp)  b)

    , let p   = 100
          m   = VSB.length vb `div` 2
          n   = m - p
          o   = 2 * p
          vbp = VSB.take o (VSB.drop n vb)
          bp  =   B.take o   (B.drop n b)
      in (vbp, bp) `deepseq`
         bli "findSubstring" (nf (VSB.findSubstring vbp) vb)
                             (nf   (B.findSubstring bp)  b)

    , let s   = "the"
          vbp = VSB8.pack s
          bp  =   B8.pack s
      in (vbp, bp) `deepseq`
         bli "findSubstrings" (nf (VSB.findSubstrings vbp) vb)
                              (nf   (B.findSubstrings bp)  b)


    ----------------------------------------------------------------------------
    -- * Searching ByteStrings
    ----------------------------------------------------------------------------

    ----------------------------------------------------------------------------
    -- ** Searching by equality

    , let !a  = 255
          !a8 = w2c a
      in BOOA(elem, a, a8, vb, b, vbl, bl)

    , let !a  = 255
          !a8 = w2c a
      in BOOA(notElem, a, a8, vb, b, vbl, bl)

    ----------------------------------------------------------------------------
    -- ** Searching with a predicate

    , let p  = (==255)
          p8 = p . c2w
      in BOOA(find, p, p8, vb, b, vbl, bl)

    , let p  = p8 . w2c
          p8 = isUpper
      in BOOA(filter, p, p8, vb, b, vbl, bl)

    , let p  = isUpper . w2c
      in blo "partition" (nf  (VSB.partition p) vb)
                         (nf    (B.partition p) b)
                         (nf (VSBL.partition p) vbl)
                         (nf   (BL.partition p) bl)


    ----------------------------------------------------------------------------
    -- * Indexing ByteStrings
    ----------------------------------------------------------------------------

    , let !ix   = VSB.length vb - 1
          !ix64 = fromIntegral ix
      in blo "index" (nf   (VSB.index vb)  ix)
                     (nf     (B.index b)   ix)
                     (nf  (VSBL.index vbl) ix64)
                     (nf    (BL.index bl)  ix64)

    , let !a  = 255
          !a8 = w2c a
      in BOOA(elemIndex, a, a8, vb, b, vbl, bl)

    , let !a  = c2w a8
          !a8 = 'a'
      in BOOA(elemIndices, a, a8, vb, b, vbl, bl)

    , let !a  = 255
          !a8 = w2c a
      in BLAA(elemIndexEnd, a, a8, vb, b)

    , let p  = (==255)
          p8 = p . c2w
      in BOOA(findIndex, p, p8, vb, b, vbl, bl)

    , let p  = p8 . w2c
          p8 = isUpper
      in BOOA(findIndices, p, p8, vb, b, vbl, bl)

    , let !c  = c2w c8
          !c8 = 'a'
      in BOOA(count, c, c8, vb, b, vbl, bl)


    ----------------------------------------------------------------------------
    -- * Zipping and unzipping ByteStrings
    ----------------------------------------------------------------------------

    , boo "zip" (nf   (VSB.zip vb)  vb)
                (nf     (B.zip b)   b)
                (nf  (VSB8.zip vb)  vb)
                (nf    (B8.zip b)   b)
                (nf  (VSBL.zip vbl) vbl)
                (nf    (BL.zip bl)  bl)
                (nf (VSBL8.zip vbl) vbl)
                (nf   (BL8.zip bl)  bl)

    , let f  x y = fromIntegral x + fromIntegral y :: Int
          f8 x y = f (c2w x) (c2w y)
      in boo "zipWith" (nf   (VSB.zipWith f  vb)  vb)
                       (nf     (B.zipWith f  b)   b)
                       (nf  (VSB8.zipWith f8 vb)  vb)
                       (nf    (B8.zipWith f8 b)   b)
                       (nf  (VSBL.zipWith f  vbl) vbl)
                       (nf    (BL.zipWith f  bl)  bl)
                       (nf (VSBL8.zipWith f8 vbl) vbl)
                       (nf   (BL8.zipWith f8 bl)  bl)

      -- See if the RULE "ByteString specialise zipWith" fires:
    , let f  x y = x + y :: Word8
          f8 x y = f (c2w x) (c2w y)
      in boo "zipWith_Word8"
                       (nf   (VSB.zipWith f  vb)  vb) -- TODO: Does the RULE fire?
                       (nf     (B.zipWith f  b)   b)
                       (nf  (VSB8.zipWith f8 vb)  vb)
                       (nf    (B8.zipWith f8 b)   b)
                       (nf  (VSBL.zipWith f  vbl) vbl)
                       (nf    (BL.zipWith f  bl)  bl)
                       (nf (VSBL8.zipWith f8 vbl) vbl)
                       (nf   (BL8.zipWith f8 bl)  bl)

    , let xs  =  VSB.zip vb vb
          xs8 = VSB8.zip vb vb
      in (xs, xs8) `deepseq`
         bgroup "unzip"
         [ bgroup "strict" $ foo  (nf   VSB.unzip xs)
                                  (nf     B.unzip xs)
                                  (nf  VSB8.unzip xs8)
                                  (nf    B8.unzip xs8)
         , bgroup "lazy"
           [ bgroup "Word8" $ bar (nf  VSBL.unzip xs)
                                  (nf    BL.unzip xs)
           ]
         ]


    ----------------------------------------------------------------------------
    -- * Ordered ByteStrings
    ----------------------------------------------------------------------------

    , bli "sort" (nf VSB.sort vb)
                 (nf   B.sort b)


    ----------------------------------------------------------------------------
    -- * Low level conversions
    ----------------------------------------------------------------------------

    , BLO(copy, vb, b, vbl, bl)

    ----------------------------------------------------------------------------
    --  ** Packing 'CString's and pointers

    , let str = VSB8.unpack vb -- "I'm going to be a CString, Yippy!!"
          doPackCString packCString =
              withCString str $ \cStr ->
                  packCString cStr >>= deepEvaluate
      in str `deepseq`
         bli "packCString" (doPackCString VSB.packCString)
                           (doPackCString   B.packCString)

    , let str = VSB8.unpack vb -- "I'm going to be a CString, Yippy!!"
          doPackCStringLen packCStringLen =
              withCStringLen str $ \cStrLen ->
                  packCStringLen cStrLen >>= deepEvaluate
      in str `deepseq`
         bli "packCStringLen" (doPackCStringLen VSB.packCStringLen)
                              (doPackCStringLen   B.packCStringLen)

    ----------------------------------------------------------------------------
    -- ** Using ByteStrings as 'CString's

    , let f _ = return ()
      in bli "useAsCString" (VSB.useAsCString vb f)
                            (  B.useAsCString  b f)

    , let f _ = return ()
      in bli "useAsCStringLen" (VSB.useAsCStringLen vb f)
                               (  B.useAsCStringLen  b f)


    ----------------------------------------------------------------------------
    --  * I\/O with 'ByteString's
    ----------------------------------------------------------------------------

    , let doReadFile readF = readF dict >>= deepEvaluate
      in blo "readFile" (doReadFile  VSB.readFile)
                        (doReadFile    B.readFile)
                        (doReadFile VSBL.readFile)
                        (doReadFile   BL.readFile)

    , let devnull = "/dev/null"
      in blo "writeFile" ( VSB.writeFile devnull vb)
                         (   B.writeFile devnull b)
                         (VSBL.writeFile devnull vbl)
                         (  BL.writeFile devnull bl)

    , let doHGetContents f = withFile dict ReadMode $ \h -> f h >>= deepEvaluate
      in blo "hGetContents" (doHGetContents  VSB.hGetContents)
                            (doHGetContents    B.hGetContents)
                            (doHGetContents VSBL.hGetContents)
                            (doHGetContents   BL.hGetContents)


    ----------------------------------------------------------------------------
    -- * Low level introduction and elimination
    ----------------------------------------------------------------------------

    , let !n  = 1000000
          f _ = return ()
          doCreate create = create n f >>= deepEvaluate
      in bli "create" (doCreate VSBI.create)
                      (doCreate   BI.create)

    , let !n  = 1000000
          f _ = return 500000
          doCreateAndTrim createAndTrim = createAndTrim n f >>= deepEvaluate
      in bli "createAndTrim" (doCreateAndTrim VSBI.createAndTrim)
                             (doCreateAndTrim   BI.createAndTrim)


    ----------------------------------------------------------------------------
    -- * Unchecked access
    ----------------------------------------------------------------------------

    , bli "unsafeHead" (nf VSBU.unsafeHead vb)
                       (nf   BU.unsafeHead  b)

    , bli "unsafeTail" (nf VSBU.unsafeTail vb)
                       (nf   BU.unsafeTail  b)

    , let !ix = 1000
      in bli "unsafeIndex" (nf (VSBU.unsafeIndex vb) ix)
                           (nf   (BU.unsafeIndex  b) ix)

    , let !n = VSB.length vb `div` 2
      in bli "unsafeTake" (nf (VSBU.unsafeTake n) vb)
                          (nf   (BU.unsafeTake n) b)

    , let !n = VSB.length vb `div` 2
      in bli "unsafeDrop" (nf (VSBU.unsafeDrop n) vb)
                          (nf   (BU.unsafeDrop n) b)


    ----------------------------------------------------------------------------
    -- Benchmarking fusion
    ----------------------------------------------------------------------------

    , bgroup "fusion" $
      let fuse name f g = bgroup name $ bar (nf f vb)
                                            (nf g b)
      in [ bgroup "non_directional"
           [ fuse "map-map"       (VSB.map (*2) . VSB.map (+4))
                                  (  B.map (*2) .   B.map (+4))
           , fuse "filter-filter" (VSB.filter (/=101) . VSB.filter (/=102))
                                  (  B.filter (/=101) .   B.filter (/=102))
           , fuse "filter-map"    (VSB.filter (/=103) . VSB.map (+5))
                                  (  B.filter (/=103) .   B.map (+5))
           , fuse "map-filter"    (VSB.map (*3) . VSB.filter (/=104))
                                  (  B.map (*3) .   B.filter (/=104))
           , fuse "map-noacc"     ((VSB.map (+1) . VSB.filter (/=112)) . VSB.map (*2))
                                  ((  B.map (+1) .   B.filter (/=112)) .   B.map (*2))
           , fuse "noacc-map"     (VSB.map (+1) . (VSB.map (+2) . VSB.filter (/=113)))
                                  (  B.map (+1) . (  B.map (+2) .   B.filter (/=113)))
           , fuse "filter-noacc"  ((VSB.map (+1) . VSB.filter (/=101)) . VSB.filter (/=114))
                                  ((  B.map (+1) .   B.filter (/=101)) .   B.filter (/=114))
           , fuse "noacc-filter"  (VSB.filter (/=101) . (VSB.map (*2) . VSB.filter (/=115)))
                                  (  B.filter (/=101) . (  B.map (*2) .   B.filter (/=115)))
           , fuse "noacc-noacc"   ((VSB.map (*3) . VSB.filter (/=108)) . (VSB.map (*4) . VSB.filter (/=109)))
                                  ((  B.map (*3) .   B.filter (/=108)) . (  B.map (*4) .   B.filter (/=109)))
           ]

         , bgroup "up_loops"
           [ fuse "up-up"          (VSB.foldl' (const.(+1)) (0::Int) . VSB.scanl (flip const) (0::Word8))
                                   (  B.foldl' (const.(+1)) (0::Int) .   B.scanl (flip const) (0::Word8))
           , fuse "map-up"         (VSB.foldl' (const.(+6)) (0::Int) . VSB.map (*4))
                                   (  B.foldl' (const.(+6)) (0::Int) .   B.map (*4))
           , fuse "up-map"         (VSB.map (+7) . VSB.scanl const (0::Word8))
                                   (  B.map (+7) .   B.scanl const (0::Word8))
           , fuse "filter-up"      (VSB.foldl' (const.(+8)) (0::Int) . VSB.filter (/=105))
                                   (  B.foldl' (const.(+8)) (0::Int) .   B.filter (/=105))
           , fuse "up-filter"      (VSB.filter (/=106) . VSB.scanl (flip const) (0::Word8))
                                   (  B.filter (/=106) .   B.scanl (flip const) (0::Word8))
           , fuse "noacc-up"       (VSB.foldl' (const.(+1)) (0::Word8) . (VSB.map (+1) . VSB.filter (/=110)))
                                   (  B.foldl' (const.(+1)) (0::Word8) . (  B.map (+1) .   B.filter (/=110)))
           , fuse "up-noacc"       ((VSB.map (+1) . VSB.filter (/=111)) . VSB.scanl (flip const) (0::Word8))
                                   ((  B.map (+1) .   B.filter (/=111)) .   B.scanl (flip const) (0::Word8))
           ]

         , bgroup "down_loops"
           [ fuse "down-down"      (VSB.foldr (const (+9))  (0::Word8) . VSB.scanr const (0::Word8))
                                   (  B.foldr (const (+9))  (0::Word8) .   B.scanr const (0::Word8))
           , fuse "map-down"       (VSB.foldr (const (+10)) (0::Word8) . VSB.map (*2))
                                   (  B.foldr (const (+10)) (0::Word8) .   B.map (*2))
           , fuse "down-map"       (VSB.map (*2) . VSB.scanr const (0::Word8))
                                   (  B.map (*2) .   B.scanr const (0::Word8))
           , fuse "filter-down"    (VSB.foldr (const (+11)) (0::Word8) . VSB.filter (/=106))
                                   (  B.foldr (const (+11)) (0::Word8) .   B.filter (/=106))
           , fuse "down-filter"    (VSB.filter (/=107) . VSB.scanr const (0::Word8))
                                   (  B.filter (/=107) .   B.scanr const (0::Word8))
           , fuse "noacc-down"     (VSB.foldr (const (+1)) (0::Word8) . (VSB.map (+1) . VSB.filter (/=116)))
                                   (  B.foldr (const (+1)) (0::Word8) . (  B.map (+1) .   B.filter (/=116)))
           , fuse "down-noacc"     ((VSB.map (+1) . VSB.filter (/=101)) . VSB.scanr const (0::Word8))
                                   ((  B.map (+1) .   B.filter (/=101)) .   B.scanr const (0::Word8))
           ]

         , bgroup "misc"
           [ fuse "length-loop"    (VSB.length  . VSB.filter (/=105))
                                   (  B.length  .   B.filter (/=105))
           , fuse "maximum-loop"   (VSB.maximum . VSB.map (*4))
                                   (  B.maximum .   B.map (*4))
           , fuse "minimum-loop"   (VSB.minimum . VSB.map (+6))
                                   (  B.minimum .   B.map (+6))
           ]

         , bgroup "big"
           [ fuse "big_map-map"       (VSB.map (subtract 3) . VSB.map (+7) . VSB.map (*2) . VSB.map (+4))
                                      (  B.map (subtract 3) .   B.map (+7) .   B.map (*2) .   B.map (+4))
           , fuse "big_filter-filter" (VSB.filter (/=103) . VSB.filter (/=104) . VSB.filter (/=101) . VSB.filter (/=102))
                                      (  B.filter (/=103) .   B.filter (/=104) .   B.filter (/=101) .   B.filter (/=102))
           , fuse "big_filter-map"    (VSB.map (*2) . VSB.filter (/=104) . VSB.map (+6) . VSB.filter (/=103) . VSB.map (+5))
                                      (  B.map (*2) .   B.filter (/=104) .   B.map (+6) .   B.filter (/=103) .   B.map (+5))
           ]
         ]


    ----------------------------------------------------------------------------
    -- Benchmarking "real world" programs
    ----------------------------------------------------------------------------

    , bgroup "real_world"
      [ bli "letter_freq" ( VSB.readFile dict >>=
                              deepEvaluate . sortBy (\x y -> snd y `compare` snd x)
                                           . map (\x -> (w2c . VSBU.unsafeHead $ x, VSB8.length x))
                                           . VSB8.group
                                           . VSB8.sort
                                           . VSB8.map toLower
                                           . VSB8.filter isAlpha )
                         ( B.readFile dict >>=
                              deepEvaluate . sortBy (\x y -> snd y `compare` snd x)
                                           . map (\x -> (w2c . BU.unsafeHead $ x, B8.length x))
                                           . B8.group
                                           . B8.sort
                                           . B8.map toLower
                                           . B8.filter isAlpha )
      ]
    ]


--------------------------------------------------------------------------------
-- Grouping
--------------------------------------------------------------------------------

boo :: Benchmarkable b => String -> b -> b -> b -> b -> b -> b -> b -> b -> Benchmark
boo name vb   b
         vb8  b8
         vbl  bl
         vbl8 bl8 =
    bgroup name [ bgroup "strict" $ foo vb   b
                                        vb8  b8
                , bgroup "lazy"   $ foo vbl  bl
                                        vbl8 bl8
                ]

blo :: Benchmarkable b => String -> b -> b -> b -> b -> Benchmark
blo name vb b
         vbl bl = bgroup name [ bgroup "strict" $ bar vb   b
                              , bgroup "lazy"   $ bar vbl  bl
                              ]

bla :: Benchmarkable b => String -> b -> b -> b -> b -> Benchmark
bla name vb  b
         vb8 b8 = bgroup name [ bgroup "strict" $ foo vb   b
                                                      vb8  b8
                              ]

bli :: Benchmarkable b => String -> b -> b -> Benchmark
bli name vb b = bgroup name $ bar vb b

--------------------------------------------------------------------------------

foo :: Benchmarkable b => b -> b -> b -> b -> [Benchmark]
foo vb  b
    vb8 b8 = [ bgroup "Word8" $ bar vb  b
             , bgroup "Char8" $ bar vb8 b8
             ]

bar :: Benchmarkable b => b -> b -> [Benchmark]
bar vb b = [ bcompare [ bench "vector"     vb
                      , bench "bytestring" b
                      ]
           ]
