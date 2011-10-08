{-# LANGUAGE CPP, BangPatterns #-}

-- Disable warnings for the orphaned NFData instances for legacy ByteStrings:
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Disable warnings for the deprecated findSubstring and findSubstrings:
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Main where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Word         ( Word8 )
import Data.Char         ( isUpper )
import Data.Monoid       ( mappend )

import qualified Data.List as List ( replicate )

-- from deepseq:
import Control.DeepSeq ( NFData, rnf )

-- from criterion:
import Criterion.Main ( Benchmarkable, Benchmark, defaultMain, bgroup, bench, nf )

-- from vector-bytestring:
import qualified Data.Vector.Storable.ByteString             as VSB
import qualified Data.Vector.Storable.ByteString.Lazy        as VSBL
import qualified Data.Vector.Storable.ByteString.Char8       as VSB8
import qualified Data.Vector.Storable.ByteString.Lazy.Char8  as VSBL8

import Data.Vector.Storable.ByteString.Internal ( c2w, w2c )

-- from bytestring:
import qualified Data.ByteString                             as B
import qualified Data.ByteString.Lazy                        as BL
import qualified Data.ByteString.Char8                       as B8
import qualified Data.ByteString.Lazy.Char8                  as BL8

import qualified Data.ByteString.Lazy.Internal as BLI


--------------------------------------------------------------------------------
-- Handy CPP macros
--------------------------------------------------------------------------------

#define BOO8(name, vb, b, vb8, b8, vbl, bl, vbl8, bl8) \
        boo "name" (nf   VSB.name vb)   \
                   (nf     B.name b)    \
                   (nf  VSB8.name vb8)  \
                   (nf    B8.name b8)   \
                   (nf  VSBL.name vbl)  \
                   (nf    BL.name bl)   \
                   (nf VSBL8.name vbl8) \
                   (nf   BL8.name bl8)

#define BOO4(name, vb, b, vbl, bl) BOO8(name, vb, b, vb, b, vbl, bl, vbl, bl)
#define BOO2(name, a, a8)          BOO8(name, a, a, a8, a8, a, a, a8, a8)

#define BOOA(name, a, a8, vb, b, vbl, bl)   \
        boo "name" (nf   (VSB.name a)  vb)  \
                   (nf     (B.name a)  b)   \
                   (nf  (VSB8.name a8) vb)  \
                   (nf    (B8.name a8) b)   \
                   (nf  (VSBL.name a)  vbl) \
                   (nf    (BL.name a)  bl)  \
                   (nf (VSBL8.name a8) vbl) \
                   (nf   (BL8.name a8) bl)  \

#define BOOSL(name, s, l, vb, b, vbl, bl)  \
        boo "name" (nf   (VSB.name s) vb)  \
                   (nf     (B.name s) b)   \
                   (nf  (VSB8.name s) vb)  \
                   (nf    (B8.name s) b)   \
                   (nf  (VSBL.name l) vbl) \
                   (nf    (BL.name l) bl)  \
                   (nf (VSBL8.name l) vbl) \
                   (nf   (BL8.name l) bl)  \

#define BOOBIN(name,   vb1, vb2,   b1, b2,   vbl1, vbl2,   bl1, bl2) \
        boo "name" (nf   (VSB.name vb1)  vb2)  \
                   (nf     (B.name b1)   b2)   \
                   (nf  (VSB8.name vb1)  vb2)  \
                   (nf    (B8.name b1)   b2)   \
                   (nf  (VSBL.name vbl1) vbl2) \
                   (nf    (BL.name bl1)  bl2)  \
                   (nf (VSBL8.name vbl1) vbl2) \
                   (nf   (BL8.name bl1)  bl2)  \

#define BOOB(name, a, a8, vb, b, vbl, bl)   \
        boo "name" (nf   (VSB.name vb)   a) \
                   (nf     (B.name b)    a) \
                   (nf  (VSB8.name vb)  a8) \
                   (nf    (B8.name b)   a8) \
                   (nf  (VSBL.name vbl)  a) \
                   (nf    (BL.name bl)   a) \
                   (nf (VSBL8.name vbl) a8) \
                   (nf   (BL8.name bl)  a8)

#define BLAA(name, a, a8, vb, b)          \
        bla "name" (nf  (VSB.name a)  vb) \
                   (nf    (B.name a)  b)  \
                   (nf (VSB8.name a8) vb) \
                   (nf   (B8.name a8) b)


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  let dict = "tests/data"

  vb    <- VSB.readFile dict
  b     <-   B.readFile dict
  vbl   <-VSBL.readFile dict
  bl    <-  BL.readFile dict

  rnf (vb, b, vbl, bl) `seq`
    defaultMain $
    [
    ----------------------------------------------------------------------------
    -- * Introducing and eliminating 'ByteString's
    ----------------------------------------------------------------------------

      let !z  = 0
          !z8 = w2c z
      in BOO2(singleton, z, z8)

    , let xs =  B.unpack b
          cs = B8.unpack b
      in rnf (xs, cs) `seq`
         BOO2(pack, xs, cs)

    , BOO4(unpack, vb, b, vbl, bl)


    ----------------------------------------------------------------------------
    --  * Basic interface
    ----------------------------------------------------------------------------

    , let !z  = 0
          !z8 = w2c z
      in BOOA(cons, z, z8, vb, b, vbl, bl)

    , let !z  = 0
          !z8 = w2c z
      in BOOB(snoc, z, z8, vb, b, vbl, bl)

    , BOOBIN(append,   vb, vb,   b, b,   vbl, vbl,   bl, bl)

    , BOO4(head,   vb, b, vbl, bl)
    , BOO4(tail,   vb, b, vbl, bl)
    , BOO4(uncons, vb, b, vbl, bl)
    , BOO4(last,   vb, b, vbl, bl)
    , BOO4(init,   vb, b, vbl, bl)
    , BOO4(null,   vb, b, vbl, bl)
    , BOO4(length, vb, b, vbl, bl)


    ----------------------------------------------------------------------------
    -- * Transforming ByteStrings
    ----------------------------------------------------------------------------

    , let mapF  = (+1)
          mapF8 = w2c . mapF . c2w
      in BOOA(map, mapF, mapF8, vb, b, vbl, bl)

    , BOO4(reverse, vb, b, vbl, bl)

    , let !z  = 0
          !z8 = w2c z
      in BOOA(intersperse, z, z8,  vb, b, vbl, bl)

    , let n = 100
          vbsN   = List.replicate n vb
          bsN    = List.replicate n b
          vblsN  = List.replicate n vbl
          blsN   = List.replicate n bl
      in rnf (vbsN, bsN, vblsN, blsN) `seq`
         BOOBIN(intercalate,   vb, vbsN,   b, bsN,   vbl, vblsN,   bl, blsN)

    , let m = 5
          vbsM   = List.replicate m vb
          bsM    = List.replicate m b
          vblsM  = List.replicate m vbl
          blsM   = List.replicate m bl
      in rnf (vbsM, bsM, vblsM, blsM) `seq`
         BOO4(transpose, vbsM, bsM, vblsM, blsM)


    ----------------------------------------------------------------------------
    -- * Reducing 'ByteString's (folds)
    ----------------------------------------------------------------------------

    , let
          foldlF  y x = y + x
          foldlF8 y x = w2c $ foldlF (c2w y) (c2w x)
          z  = 0
          z8 = w2c z

          -- TODO:
          -- Enabling these arguments instead of the former causes GHC to loop!!!
          -- foldlF  xs x = x:xs
          -- foldlF8 xs x = x:xs
          -- z  = []
          -- z8 = []

      in BOOA(foldl, foldlF z, foldlF8 z8, vb, b, vbl, bl)

    , let foldl'F  y x = x + y
          foldl'F8 y x = w2c $ foldl'F (c2w y) (c2w x)
          !z  = 0
          !z8 = w2c z
      in boo "foldl'" (nf   (VSB.foldl' foldl'F  z)  vb)
                      (nf     (B.foldl' foldl'F  z)  b)
                      (nf  (VSB8.foldl' foldl'F8 z8) vb)
                      (nf    (B8.foldl' foldl'F8 z8) b)
                      (nf  (VSBL.foldl' foldl'F  z)  vbl)
                      (nf    (BL.foldl' foldl'F  z)  bl)
                      (nf (VSBL8.foldl' foldl'F8 z8) vbl)
                      (nf   (BL8.foldl' foldl'F8 z8) bl)

    , let foldl1F  y x = x + y
          foldl1F8 y x = w2c $ foldl1F (c2w y) (c2w x)
          n    = 100000
          n64  = fromIntegral n
          vb2  =  VSB.take n   vb
          b2   =    B.take n   b
          vbl2 = VSBL.take n64 vbl
          bl2  =   BL.take n64 bl
      in rnf (vb2, b2, vbl2, bl2) `seq`
         BOOA(foldl1, foldl1F, foldl1F8, vb2, b2, vbl2, bl2)

    , let foldl1'F  y x = x + y
          foldl1'F8 y x = w2c $ foldl1'F (c2w y) (c2w x)
      in boo "foldl1'" (nf   (VSB.foldl1' foldl1'F)  vb)
                       (nf     (B.foldl1' foldl1'F)  b)
                       (nf  (VSB8.foldl1' foldl1'F8) vb)
                       (nf    (B8.foldl1' foldl1'F8) b)
                       (nf  (VSBL.foldl1' foldl1'F)  vbl)
                       (nf    (BL.foldl1' foldl1'F)  bl)
                       (nf (VSBL8.foldl1' foldl1'F8) vbl)
                       (nf   (BL8.foldl1' foldl1'F8) bl)

    , let foldrF  = (:)
          foldrF8 = (:)
          z  = []
          z8 = []
      in BOOA(foldr, foldrF  z, foldrF8 z8, vb, b, vbl, bl)

    , let foldr'F  y x = x + y
          foldr'F8 y x = w2c $ foldr'F (c2w y) (c2w x)
          !z  = 0
          !z8 = w2c z
      in bla "foldr'" (nf   (VSB.foldr' foldr'F  z)  vb)
                      (nf     (B.foldr' foldr'F  z)  b)
                      (nf  (VSB8.foldr' foldr'F8 z8) vb)
                      (nf    (B8.foldr' foldr'F8 z8) b)

    , let foldr1F  y x = x + y
          foldr1F8 y x = w2c $ foldr1F (c2w y) (c2w x)
          n    = 100000
          n64  = fromIntegral n
          vb2  =  VSB.take n   vb
          b2   =    B.take n   b
          vbl2 = VSBL.take n64 vbl
          bl2  =   BL.take n64 bl
      in rnf (vb2, b2, vbl2, bl2) `seq`
         BOOA(foldr1, foldr1F, foldr1F8, vb2, b2, vbl2, bl2)

    , let foldr1'F  y x = x + y
          foldr1'F8 y x = w2c $ foldr1'F (c2w y) (c2w x)
      in bla "foldr1'" (nf   (VSB.foldr1' foldr1'F)  vb)
                       (nf     (B.foldr1' foldr1'F)  b)
                       (nf  (VSB8.foldr1' foldr1'F8) vb)
                       (nf    (B8.foldr1' foldr1'F8) b)

    ----------------------------------------------------------------------------
    -- ** Special folds

    , let m = 5
          vbsM   = List.replicate m vb
          bsM    = List.replicate m b
          vblsM  = List.replicate m vbl
          blsM   = List.replicate m bl
      in rnf (vbsM, bsM, vblsM, blsM) `seq`
         BOO4(concat, vbsM, bsM, vblsM, blsM)

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

    , let anyF  = (== 255)
          anyF8 = anyF . c2w
      in BOOA(any, anyF, anyF8, vb, b, vbl, bl)

    , let allF  = (<= 255)
          allF8 = allF . c2w
      in BOOA(all, allF, allF8, vb, b, vbl, bl)

    , BOO4(maximum, vb, b, vbl, bl)
    , BOO4(minimum, vb, b, vbl, bl)


    ----------------------------------------------------------------------------
    -- * Building ByteStrings
    ----------------------------------------------------------------------------

    ----------------------------------------------------------------------------
    -- ** Scans

    , let scanlF  x y = x + y
          scanlF8 x y = w2c $ scanlF (c2w x) (c2w y)
          !z  = 1
          !z8 = w2c z
          n = 1000 -- If you increase this you get stack space overflows in:
                   -- scanl/lazy/Word8/vector
                   -- scanl/lazy/Word8/bytestring
                   -- scanl/lazy/Char8/vector
                   -- scanl/lazy/Char8/bytestring
          n64  = fromIntegral n
          vb2  =  VSB.take n   vb
          b2   =    B.take n   b
          vbl2 = VSBL.take n64 vbl
          bl2  =   BL.take n64 bl
      in rnf (vb2, b2, vbl2, bl2) `seq`
         BOOA(scanl, scanlF z, scanlF8 z8, vb2, b2, vbl2, bl2)

    , let scanl1F  x y = x + y
          scanl1F8 x y = w2c $ scanl1F (c2w x) (c2w y)
      in BLAA(scanl1, scanl1F, scanl1F8, vb, b)

    , let scanrF  x y = x + y
          scanrF8 x y = w2c $ scanrF (c2w x) (c2w y)
          !z  = 1
          !z8 = w2c z
      in BLAA(scanr, scanrF z, scanrF8 z8, vb, b)

    , let scanr1F  x y = x + y
          scanr1F8 x y = w2c $ scanr1F (c2w x) (c2w y)
      in BLAA(scanr1, scanr1F, scanr1F8, vb, b)

    ----------------------------------------------------------------------------
    -- ** Accumulating maps

    , let mapAccumLF  acc x = (x:acc, x * x + x)
          mapAccumLF8 acc c = (c:acc, w2c $ c2w c * c2w c + c2w c)
      in BOOA(mapAccumL, mapAccumLF [], mapAccumLF8 [], vb, b, vbl, bl)

    , let mapAccumRF  acc x = (x:acc, x * x + x)
          mapAccumRF8 acc c = (c:acc, w2c $ c2w c * c2w c + c2w c)
      in BOOA(mapAccumR, mapAccumRF [], mapAccumRF8 [], vb, b, vbl, bl)

    ----------------------------------------------------------------------------
    -- ** Generating and unfolding ByteStrings

    , let !o   = 1000000
          !o64 = fromIntegral o
          !z   = 0
          !z8  = w2c z
      in BOOB(replicate, z, z8, o, o, o64, o64)
    ]
    ++
    ( let unfoldrF :: Int -> Maybe (Word8, Int)
          unfoldrF 1000000 = Nothing
          unfoldrF i       = Just (fromIntegral i, i+1)

          unfoldrF8 :: Int -> Maybe (Char, Int)
          unfoldrF8 1000000 = Nothing
          unfoldrF8 i       = Just (w2c $ fromIntegral i, i+1)

      in [ BOOA(unfoldr, unfoldrF, unfoldrF8, 0, 0, 0, 0)
         , let !k = 1000000
           in BLAA(unfoldrN, k unfoldrF, k unfoldrF8, 0, 0)
         ]
    ) ++


    ----------------------------------------------------------------------------
    -- * Substrings
    ----------------------------------------------------------------------------

    ----------------------------------------------------------------------------
    -- ** Breaking strings

    [ let !t   = 260000
          !t64 = fromIntegral t
      in BOOSL(take, t, t64, vb, b, vbl, bl)

    , let !d   = 10000
          !d64 = fromIntegral d
      in BOOSL(drop, d, d64, vb, b, vbl, bl)

    , let !s   = 260000 `div` 2
          !s64 = fromIntegral s
      in BOOSL(splitAt, s, s64, vb, b, vbl, bl)

    , let takeWhileF  = (<= 255)         -- take everything
          takeWhileF8 = takeWhileF . c2w -- take everything
      in BOOA(takeWhile, takeWhileF, takeWhileF8, vb, b, vbl, bl)
      -- takeWhile/strict/Char8/vector is suspiciously fast!

    , let dropWhileF  = (<= 255)         -- drop everything
          dropWhileF8 = dropWhileF . c2w -- drop everything
      in BOOA(dropWhile, dropWhileF, dropWhileF8, vb, b, vbl, bl)
      -- dropWhile/strict/Char8/vector is suspiciously fast!

    , let spanF  = (<= 255)    -- span till end
          spanF8 = spanF . c2w -- span till end
      in BOOA(span, spanF, spanF8, vb, b, vbl, bl)

      -- See if the RULE: "ByteString specialise span (x==)" fires:
    , let spanEqF  = (==p)
          spanEqF8 = spanEqF . c2w
          rn   = 500000
          rn64 = fromIntegral rn
          p = 1
          q = 2
          vbSpan  =  VSB.replicate rn   p `mappend`  VSB.replicate rn   q
          bSpan   =    B.replicate rn   p `mappend`    B.replicate rn   q
          vblSpan = VSBL.replicate rn64 p `mappend` VSBL.replicate rn64 q
          blSpan  =   BL.replicate rn64 p `mappend`   BL.replicate rn64 q
      in rnf (vbSpan, bSpan, vblSpan, blSpan) `seq`
         boo "span_eq" (nf   (VSB.span spanEqF)  vbSpan)
                       (nf     (B.span spanEqF)  bSpan)
                       (nf  (VSB8.span spanEqF8) vbSpan)
                       (nf    (B8.span spanEqF8) bSpan)
                       (nf  (VSBL.span spanEqF)  vblSpan)
                       (nf    (BL.span spanEqF)  blSpan)
                       (nf (VSBL8.span spanEqF8) vblSpan)
                       (nf   (BL8.span spanEqF8) blSpan)

    , let spanF  = (<= 255)
          spanF8 = spanF . c2w
      in BLAA(spanEnd, spanF, spanF8, vb, b)

    , let breakF  = (>= 255)
          breakF8 = breakF . c2w
      in BOOA(break, breakF, breakF8, vb, b, vbl, bl)

      -- See if the RULE: "ByteString specialise break (x==)" fires:
    , let breakEqF  = (==q)
          breakEqF8 = breakEqF . c2w
          rn   = 500000
          rn64 = fromIntegral rn
          p = 1
          q = 2
          vbSpan  =  VSB.replicate rn   p `mappend`  VSB.replicate rn   q
          bSpan   =    B.replicate rn   p `mappend`    B.replicate rn   q
          vblSpan = VSBL.replicate rn64 p `mappend` VSBL.replicate rn64 q
          blSpan  =   BL.replicate rn64 p `mappend`   BL.replicate rn64 q
      in rnf (vbSpan, bSpan, vblSpan, blSpan) `seq`
         boo "break_eq" (nf   (VSB.break breakEqF)  vbSpan)
                        (nf     (B.break breakEqF)  bSpan)
                        (nf  (VSB8.break breakEqF8) vbSpan)
                        (nf    (B8.break breakEqF8) bSpan)
                        (nf  (VSBL.break breakEqF)  vblSpan)
                        (nf    (BL.break breakEqF)  blSpan)
                        (nf (VSBL8.break breakEqF8) vblSpan)
                        (nf   (BL8.break breakEqF8) blSpan)

    , let breakF  = (>= 255)
          breakF8 = breakF . c2w
      in BLAA(breakEnd, breakF, breakF8, vb, b)

    , BOO4(group, vb, b, vbl, bl)

    , let groupByF  x y = x < y
          groupByF8 x y = groupByF (c2w x) (c2w y)
      in BOOA(groupBy, groupByF, groupByF8, vb, b, vbl, bl)

    , BOO4(inits, vb, b, vbl, bl)
    , BOO4(tails, vb, b, vbl, bl)

    ----------------------------------------------------------------------------
    -- ** Breaking into many substrings

    , let !nlWord = c2w nlChar
          !nlChar = '\n'
      in BOOA(split, nlWord, nlChar, vb, b, vbl, bl)

    , let splitWithF  = splitWithF8 . w2c
          splitWithF8 = (=='a')
      in BOOA(splitWith, splitWithF, splitWithF8, vb, b, vbl, bl)


    ----------------------------------------------------------------------------
    -- * Predicates
    ----------------------------------------------------------------------------

    , let p    = 1
          p64  = fromIntegral p
          vbp  =  VSB.take p   vb
          bp   =    B.take p   b
          vblp = VSBL.take p64 vbl
          blp  =   BL.take p64 bl
      in rnf (vbp, bp, vblp, blp) `seq`
         BOOBIN(isPrefixOf,   vbp, vb,   bp, b,   vblp, vbl,   blp, bl)

    , let p    = VSB.length vb - 1
          vbp  = VSB.drop p vb
          bp   =   B.drop p b
      in rnf (vbp, bp) `seq`
         bla "isSuffixOf" (nf  (VSB.isSuffixOf vbp) vb)
                          (nf    (B.isSuffixOf bp)  b)
                          (nf (VSB8.isSuffixOf vbp) vb)
                          (nf   (B8.isSuffixOf bp)  b)

    , let p   = 100
          m   = VSB.length vb `div` 2
          n   = m - p
          o   = 2 * p
          vbp = VSB.take o (VSB.drop n vb)
          bp  =   B.take o   (B.drop n b)
      in rnf (vbp, bp) `seq`
         bla "isInfixOf" (nf  (VSB.isInfixOf vbp) vb)
                         (nf    (B.isInfixOf bp)  b)
                         (nf (VSB8.isInfixOf vbp) vb)
                         (nf   (B8.isInfixOf bp)  b)

    ----------------------------------------------------------------------------
    --  ** Search for arbitrary substrings

    , let p   = 100
          m   = VSB.length vb `div` 2
          n   = m - p
          o   = 2 * p
          vbp = VSB.take o (VSB.drop n vb)
          bp  =   B.take o   (B.drop n b)
      in rnf (vbp, bp) `seq`
         bla "breakSubstring" (nf  (VSB.breakSubstring vbp) vb)
                              (nf    (B.breakSubstring bp)  b)
                              (nf (VSB8.breakSubstring vbp) vb)
                              (nf   (B8.breakSubstring bp)  b)

    , let p   = 100
          m   = VSB.length vb `div` 2
          n   = m - p
          o   = 2 * p
          vbp = VSB.take o (VSB.drop n vb)
          bp  =   B.take o   (B.drop n b)
      in rnf (vbp, bp) `seq`
         bla "findSubstring" (nf  (VSB.findSubstring vbp) vb)
                             (nf    (B.findSubstring bp)  b)
                             (nf (VSB8.findSubstring vbp) vb)
                             (nf   (B8.findSubstring bp)  b)

    , let s = "the"
          vbp = VSB8.pack s
          bp  =   B8.pack s
      in rnf (vbp, bp) `seq`
         bla "findSubstrings" (nf  (VSB.findSubstrings vbp) vb)
                              (nf    (B.findSubstrings bp)  b)
                              (nf (VSB8.findSubstrings vbp) vb)
                              (nf   (B8.findSubstrings bp)  b)


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

    , let findF  = (==255)
          findF8 = findF . c2w
      in BOOA(find, findF, findF8, vb, b, vbl, bl)

    , let filterF  = filterF8 . w2c
          filterF8 = isUpper
      in BOOA(filter, filterF, filterF8, vb, b, vbl, bl)

    , let partitionF  = isUpper . w2c
      in blo "partition" (nf  (VSB.partition partitionF) vb)
                         (nf    (B.partition partitionF) b)
                         (nf (VSBL.partition partitionF) vbl)
                         (nf   (BL.partition partitionF) bl)


    ----------------------------------------------------------------------------
    -- * Indexing ByteStrings
    ----------------------------------------------------------------------------

    , let !ix   = VSB.length vb - 1
          !ix64 = fromIntegral ix
      in blo "index" (nf   (VSB.index vb)  ix)
                     (nf     (B.index b)   ix)
                     (nf  (VSBL.index vbl) ix64)
                     (nf    (BL.index bl)  ix64)

    -- TODO: elemIndex
    -- TODO: elemIndices
    -- TODO: elemIndexEnd
    -- TODO: findIndex
    -- TODO: findIndices

    , let !c  = c2w c8
          !c8 = 'a'
      in BOOA(count, c, c8, vb, b, vbl, bl)

    ----------------------------------------------------------------------------
    -- * Zipping and unzipping ByteStrings
    ----------------------------------------------------------------------------

    , BOOBIN(zip,   vb, vb,   b, b,   vbl, vbl,   bl, bl)

    -- TODO: zipWith
    -- TODO: zipWith_Word8
    -- TODO: unzip


    ----------------------------------------------------------------------------
    -- * Ordered ByteStrings
    ----------------------------------------------------------------------------

    , bla "sort" (nf  VSB.sort vb)
                 (nf    B.sort b)
                 (nf VSB8.sort vb)
                 (nf   B8.sort b)

    ----------------------------------------------------------------------------
    -- * Low level conversions
    ----------------------------------------------------------------------------

    , BOO4(copy, vb, b, vbl, bl)

    ----------------------------------------------------------------------------
    --  ** Packing 'CString's and pointers

    -- TODO: packCString
    -- TODO: packCStringLen

    ----------------------------------------------------------------------------
    -- ** Using ByteStrings as 'CString's

    -- TODO: useAsCString
    -- TODO: useAsCStringLen


    ----------------------------------------------------------------------------
    --  * I\/O with 'ByteString's
    ----------------------------------------------------------------------------

    -- TODO
    ] ++


    ----------------------------------------------------------------------------
    -- Benchmarking fusion
    ----------------------------------------------------------------------------

    [ bgroup "fusion" $
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

foo :: Benchmarkable b => b -> b -> b -> b -> [Benchmark]
foo vb  b
    vb8 b8 = [ bgroup "Word8" $ bar vb  b
             , bgroup "Char8" $ bar vb8 b8
             ]

bar :: Benchmarkable b => b -> b -> [Benchmark]
bar vb b = [ bench "vector"     vb
           , bench "bytestring" b
           ]


--------------------------------------------------------------------------------
-- Orphaned NFData instances for legacy ByteStrings
--------------------------------------------------------------------------------

instance NFData B.ByteString

instance NFData BL.ByteString where
    rnf BLI.Empty = ()
    rnf (BLI.Chunk c cs) = rnf c `seq` rnf cs
