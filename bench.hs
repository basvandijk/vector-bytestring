-- Disable warnings for the orphaned NFData instances for legacy ByteStrings:
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Word         ( Word8 )
import Data.Int          ( Int64 )
import Control.Exception ( evaluate )

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
-- Utility functions and constants
--------------------------------------------------------------------------------

dict :: String
dict  = "tests/data"

-- TODO: It would be nice if this function was defined in Control.DeepSeq:
deepEvaluate :: NFData a => a -> IO ()
deepEvaluate = evaluate . rnf

o, k, t, d, s :: Int
o = 1000000   -- Used in replicate
k = 1000000   -- Used in unfoldrN
t = 260000    -- Used in take
d = 10000     -- Used in drop
s = t `div` 2 -- Used in splitAt

o64, t64, d64, s64 :: Int64
o64 = fromIntegral o
t64 = fromIntegral t
d64 = fromIntegral d
s64 = fromIntegral s

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Creating arguments..."

  vb    <-   VSB.readFile dict
  b     <-     B.readFile dict
  vb8   <-  VSB8.readFile dict
  b8    <-    B8.readFile dict
  vbl   <-  VSBL.readFile dict
  bl    <-    BL.readFile dict
  vbl8  <- VSBL8.readFile dict
  bl8   <-   BL8.readFile dict

  let xs :: [Word8]
      xs = B.unpack b

      cs :: String
      cs = B8.unpack b8

      n = 100

      vbsN   = List.replicate n vb
      bsN    = List.replicate n b
      vb8sN  = List.replicate n vb8
      b8sN   = List.replicate n b8
      vblsN  = List.replicate n vbl
      blsN   = List.replicate n bl
      vbl8sN = List.replicate n vbl8
      bl8sN  = List.replicate n bl8

      m = 5

      vbsM   = List.replicate m vb
      bsM    = List.replicate m b
      vb8sM  = List.replicate m vb8
      b8sM   = List.replicate m b8
      vblsM  = List.replicate m vbl
      blsM   = List.replicate m bl
      vbl8sM = List.replicate m vbl8
      bl8sM  = List.replicate m bl8

  putStrLn "Forcing arguments..."

  deepEvaluate vb
  deepEvaluate b
  deepEvaluate vb8
  deepEvaluate b8
  deepEvaluate vbl
  deepEvaluate bl
  deepEvaluate vbl8
  deepEvaluate bl8

  deepEvaluate xs
  deepEvaluate cs

  deepEvaluate vbsN
  deepEvaluate bsN
  deepEvaluate vb8sN
  deepEvaluate b8sN
  deepEvaluate vblsN
  deepEvaluate blsN
  deepEvaluate vbl8sN
  deepEvaluate bl8sN

  deepEvaluate vbsM
  deepEvaluate bsM
  deepEvaluate vb8sM
  deepEvaluate b8sM
  deepEvaluate vblsM
  deepEvaluate blsM
  deepEvaluate vbl8sM
  deepEvaluate bl8sM

  putStrLn "Start benchmarking..."

  defaultMain
    [ boo "singleton"   (nf   VSB.singleton 1)
                        (nf     B.singleton 1)
                        (nf  VSB8.singleton 'a')
                        (nf    B8.singleton 'a')
                        (nf  VSBL.singleton 1)
                        (nf    BL.singleton 1)
                        (nf VSBL8.singleton 'a')
                        (nf   BL8.singleton 'a')

    , boo "pack"        (nf   VSB.pack xs)
                        (nf     B.pack xs)
                        (nf  VSB8.pack cs)
                        (nf    B8.pack cs)
                        (nf  VSBL.pack xs)
                        (nf    BL.pack xs)
                        (nf VSBL8.pack cs)
                        (nf   BL8.pack cs)

    , boo "unpack"      (nf   VSB.unpack vb)
                        (nf     B.unpack b)
                        (nf  VSB8.unpack vb8)
                        (nf    B8.unpack b8)
                        (nf  VSBL.unpack vbl)
                        (nf    BL.unpack bl)
                        (nf VSBL8.unpack vbl8)
                        (nf   BL8.unpack bl8)

    , boo "cons"        (nf   (VSB.cons 1)   vb)
                        (nf     (B.cons 1)   b)
                        (nf  (VSB8.cons 'a') vb8)
                        (nf    (B8.cons 'a') b8)
                        (nf  (VSBL.cons 1)   vbl)
                        (nf    (BL.cons 1)   bl)
                        (nf (VSBL8.cons 'a') vbl8)
                        (nf   (BL8.cons 'a') bl8)

    , boo "snoc"        (nf   (VSB.snoc vb)   1)
                        (nf     (B.snoc b)    1)
                        (nf  (VSB8.snoc vb8)  'a')
                        (nf    (B8.snoc b8)   'a')
                        (nf  (VSBL.snoc vbl)  1)
                        (nf    (BL.snoc bl)   1)
                        (nf (VSBL8.snoc vbl8) 'a')
                        (nf   (BL8.snoc bl8)  'a')

    , boo "append"      (nf   (VSB.append vb)   vb)
                        (nf     (B.append b)    b)
                        (nf  (VSB8.append vb8)  vb8)
                        (nf    (B8.append b8)   b8)
                        (nf  (VSBL.append vbl)  vbl)
                        (nf    (BL.append bl)   bl)
                        (nf (VSBL8.append vbl8) vbl8)
                        (nf   (BL8.append bl8)  bl8)

    , boo "head"        (nf   VSB.head vb)
                        (nf     B.head b)
                        (nf  VSB8.head vb8)
                        (nf    B8.head b8)
                        (nf  VSBL.head vbl)
                        (nf    BL.head bl)
                        (nf VSBL8.head vbl8)
                        (nf   BL8.head bl8)

    , boo "tail"        (nf   VSB.tail vb)
                        (nf     B.tail b)
                        (nf  VSB8.tail vb8)
                        (nf    B8.tail b8)
                        (nf  VSBL.tail vbl)
                        (nf    BL.tail bl)
                        (nf VSBL8.tail vbl8)
                        (nf   BL8.tail bl8)

    , boo "uncons"      (nf   VSB.uncons vb)
                        (nf     B.uncons b)
                        (nf  VSB8.uncons vb8)
                        (nf    B8.uncons b8)
                        (nf  VSBL.uncons vbl)
                        (nf    BL.uncons bl)
                        (nf VSBL8.uncons vbl8)
                        (nf   BL8.uncons bl8)

    , boo "last"        (nf   VSB.last vb)
                        (nf     B.last b)
                        (nf  VSB8.last vb8)
                        (nf    B8.last b8)
                        (nf  VSBL.last vbl)
                        (nf    BL.last bl)
                        (nf VSBL8.last vbl8)
                        (nf   BL8.last bl8)

    , boo "init"        (nf   VSB.init vb)
                        (nf     B.init b)
                        (nf  VSB8.init vb8)
                        (nf    B8.init b8)
                        (nf  VSBL.init vbl)
                        (nf    BL.init bl)
                        (nf VSBL8.init vbl8)
                        (nf   BL8.init bl8)

    , boo "null"        (nf   VSB.null vb)
                        (nf     B.null b)
                        (nf  VSB8.null vb8)
                        (nf    B8.null b8)
                        (nf  VSBL.null vbl)
                        (nf    BL.null bl)
                        (nf VSBL8.null vbl8)
                        (nf   BL8.null bl8)

    , boo "length"      (nf   VSB.length vb)
                        (nf     B.length b)
                        (nf  VSB8.length vb8)
                        (nf    B8.length b8)
                        (nf  VSBL.length vbl)
                        (nf    BL.length bl)
                        (nf VSBL8.length vbl8)
                        (nf   BL8.length bl8)

    , boo "map"         (nf   (VSB.map mapF)  vb)
                        (nf     (B.map mapF)  b)
                        (nf  (VSB8.map mapF8) vb8)
                        (nf    (B8.map mapF8) b8)
                        (nf  (VSBL.map mapF)  vbl)
                        (nf    (BL.map mapF)  bl)
                        (nf (VSBL8.map mapF8) vbl8)
                        (nf   (BL8.map mapF8) bl8)

    , boo "reverse"     (nf   VSB.reverse vb)
                        (nf     B.reverse b)
                        (nf  VSB8.reverse vb8)
                        (nf    B8.reverse b8)
                        (nf  VSBL.reverse vbl)
                        (nf    BL.reverse bl)
                        (nf VSBL8.reverse vbl8)
                        (nf   BL8.reverse bl8)

    , boo "intersperse" (nf   (VSB.intersperse 1)   vb)
                        (nf     (B.intersperse 1)   b)
                        (nf  (VSB8.intersperse 'a') vb8)
                        (nf    (B8.intersperse 'a') b8)
                        (nf  (VSBL.intersperse 1)   vbl)
                        (nf    (BL.intersperse 1)   bl)
                        (nf (VSBL8.intersperse 'a') vbl8)
                        (nf   (BL8.intersperse 'a') bl8)

    , boo "intercalate" (nf   (VSB.intercalate vb)   vbsN)
                        (nf     (B.intercalate b)    bsN)
                        (nf  (VSB8.intercalate vb8)  vb8sN)
                        (nf    (B8.intercalate b8)   b8sN)
                        (nf  (VSBL.intercalate vbl)  vblsN)
                        (nf    (BL.intercalate bl)   blsN)
                        (nf (VSBL8.intercalate vbl8) vbl8sN)
                        (nf   (BL8.intercalate bl8)  bl8sN)

    , boo "transpose"   (nf   VSB.transpose vbsM)
                        (nf     B.transpose bsM)
                        (nf  VSB8.transpose vb8sM)
                        (nf    B8.transpose b8sM)
                        (nf  VSBL.transpose vblsM)
                        (nf    BL.transpose blsM)
                        (nf VSBL8.transpose vbl8sM)
                        (nf   BL8.transpose bl8sM)

    , boo "foldl"       (nf   (VSB.foldl foldlF  1)   vb)
                        (nf     (B.foldl foldlF  1)   b)
                        (nf  (VSB8.foldl foldlF8 'a') vb8)
                        (nf    (B8.foldl foldlF8 'a') b8)
                        (nf  (VSBL.foldl foldlF  1)   vbl)
                        (nf    (BL.foldl foldlF  1)   bl)
                        (nf (VSBL8.foldl foldlF8 'a') vbl8)
                        (nf   (BL8.foldl foldlF8 'a') bl8)

    , boo "foldl'"      (nf   (VSB.foldl' foldlF  1)   vb)
                        (nf     (B.foldl' foldlF  1)   b)
                        (nf  (VSB8.foldl' foldlF8 'a') vb8)
                        (nf    (B8.foldl' foldlF8 'a') b8)
                        (nf  (VSBL.foldl' foldlF  1)   vbl)
                        (nf    (BL.foldl' foldlF  1)   bl)
                        (nf (VSBL8.foldl' foldlF8 'a') vbl8)
                        (nf   (BL8.foldl' foldlF8 'a') bl8)

    , boo "foldl1"      (nf   (VSB.foldl1 foldlF)  vb)
                        (nf     (B.foldl1 foldlF)  b)
                        (nf  (VSB8.foldl1 foldlF8) vb8)  -- Stack space overflow!
                        (nf    (B8.foldl1 foldlF8) b8)
                        (nf  (VSBL.foldl1 foldlF)  vbl)  -- Stack space overflow!
                        (nf    (BL.foldl1 foldlF)  bl)
                        (nf (VSBL8.foldl1 foldlF8) vbl8) -- Stack space overflow!
                        (nf   (BL8.foldl1 foldlF8) bl8)

    , boo "foldl1'"     (nf   (VSB.foldl1' foldlF)  vb)
                        (nf     (B.foldl1' foldlF)  b)
                        (nf  (VSB8.foldl1' foldlF8) vb8)
                        (nf    (B8.foldl1' foldlF8) b8)
                        (nf  (VSBL.foldl1' foldlF)  vbl)
                        (nf    (BL.foldl1' foldlF)  bl)
                        (nf (VSBL8.foldl1' foldlF8) vbl8)
                        (nf   (BL8.foldl1' foldlF8) bl8)

    , boo "foldr"       (nf   (VSB.foldr foldrF  1)   vb)
                        (nf     (B.foldr foldrF  1)   b)
                        (nf  (VSB8.foldr foldrF8 'a') vb8)
                        (nf    (B8.foldr foldrF8 'a') b8)
                        (nf  (VSBL.foldr foldrF  1)   vbl)  -- Stack space overflow!
                        (nf    (BL.foldr foldrF  1)   bl)
                        (nf (VSBL8.foldr foldrF8 'a') vbl8) -- Stack space overflow!
                        (nf   (BL8.foldr foldrF8 'a') bl8)

    , bla "foldr'"      (nf   (VSB.foldr' foldrF  1)   vb)
                        (nf     (B.foldr' foldrF  1)   b)
                        (nf  (VSB8.foldr' foldrF8 'a') vb8)
                        (nf    (B8.foldr' foldrF8 'a') b8)

    , boo "foldr1"      (nf   (VSB.foldr1 foldrF)  vb)
                        (nf     (B.foldr1 foldrF)  b)
                        (nf  (VSB8.foldr1 foldrF8) vb8)  -- Stack space overflow!
                        (nf    (B8.foldr1 foldrF8) b8)
                        (nf  (VSBL.foldr1 foldrF)  vbl)  -- Stack space overflow!
                        (nf    (BL.foldr1 foldrF)  bl)
                        (nf (VSBL8.foldr1 foldrF8) vbl8) -- Stack space overflow!
                        (nf   (BL8.foldr1 foldrF8) bl8)

    , bla "foldr1'"     (nf   (VSB.foldr1' foldrF)  vb)
                        (nf     (B.foldr1' foldrF)  b)
                        (nf  (VSB8.foldr1' foldrF8) vb8)
                        (nf    (B8.foldr1' foldrF8) b8)

    , boo "concat"      (nf   VSB.concat vbsM)
                        (nf     B.concat bsM)
                        (nf  VSB8.concat vb8sM)
                        (nf    B8.concat b8sM)
                        (nf  VSBL.concat vblsM)
                        (nf    BL.concat blsM)
                        (nf VSBL8.concat vbl8sM)
                        (nf   BL8.concat bl8sM)

    , boo "concatMap"   (nf   (VSB.concatMap (  VSB.replicate 5)) vb)
                        (nf     (B.concatMap (    B.replicate 5)) b)
                        (nf  (VSB8.concatMap ( VSB8.replicate 5)) vb8)
                        (nf    (B8.concatMap (   B8.replicate 5)) b8)
                        (nf  (VSBL.concatMap ( VSBL.replicate 5)) vbl)
                        (nf    (BL.concatMap (   BL.replicate 5)) bl)
                        (nf (VSBL8.concatMap (VSBL8.replicate 5)) vbl8)
                        (nf   (BL8.concatMap (  BL8.replicate 5)) bl8)

    , boo "any"         (nf   (VSB.any anyF)  vb)
                        (nf     (B.any anyF)  b)
                        (nf  (VSB8.any anyF8) vb8)
                        (nf    (B8.any anyF8) b8)
                        (nf  (VSBL.any anyF)  vbl)
                        (nf    (BL.any anyF)  bl)
                        (nf (VSBL8.any anyF8) vbl8)
                        (nf   (BL8.any anyF8) bl8)

    , boo "all"         (nf   (VSB.all allF)  vb)
                        (nf     (B.all allF)  b)
                        (nf  (VSB8.all allF8) vb8)
                        (nf    (B8.all allF8) b8)
                        (nf  (VSBL.all allF)  vbl)
                        (nf    (BL.all allF)  bl)
                        (nf (VSBL8.all allF8) vbl8)
                        (nf   (BL8.all allF8) bl8)

    , boo "maximum"     (nf   VSB.maximum vb)
                        (nf     B.maximum b)
                        (nf  VSB8.maximum vb8)
                        (nf    B8.maximum b8)
                        (nf  VSBL.maximum vbl)
                        (nf    BL.maximum bl)
                        (nf VSBL8.maximum vbl8)
                        (nf   BL8.maximum bl8)

    , boo "minimum"     (nf   VSB.minimum vb)
                        (nf     B.minimum b)
                        (nf  VSB8.minimum vb8)
                        (nf    B8.minimum b8)
                        (nf  VSBL.minimum vbl)
                        (nf    BL.minimum bl)
                        (nf VSBL8.minimum vbl8)
                        (nf   BL8.minimum bl8)

    , boo "scanl"       (nf   (VSB.scanl scanlF  1)   vb)
                        (nf     (B.scanl scanlF  1)   b)
                        (nf  (VSB8.scanl scanlF8 'a') vb8)
                        (nf    (B8.scanl scanlF8 'a') b8)
                        (nf  (VSBL.scanl scanlF  1)   vbl)  -- Stack space overflow!
                        (nf    (BL.scanl scanlF  1)   bl)   -- Stack space overflow!
                        (nf (VSBL8.scanl scanlF8 'a') vbl8) -- Stack space overflow!
                        (nf   (BL8.scanl scanlF8 'a') bl8)  -- Stack space overflow!

    , bla "scanl1"      (nf   (VSB.scanl1 scanlF)     vb)
                        (nf     (B.scanl1 scanlF)     b)
                        (nf  (VSB8.scanl1 scanlF8)    vb8)
                        (nf    (B8.scanl1 scanlF8)    b8)

    , bla "scanr"       (nf   (VSB.scanr scanrF  1)   vb)
                        (nf     (B.scanr scanrF  1)   b)
                        (nf  (VSB8.scanr scanrF8 'a') vb8)
                        (nf    (B8.scanr scanrF8 'a') b8)

    , bla "scanr1"      (nf   (VSB.scanr1 scanrF)     vb)
                        (nf     (B.scanr1 scanrF)     b)
                        (nf  (VSB8.scanr1 scanrF8)    vb8)
                        (nf    (B8.scanr1 scanrF8)    b8)

    , boo "mapAccumL"   (nf   (VSB.mapAccumL mapAccumLF  []) vb)
                        (nf     (B.mapAccumL mapAccumLF  []) b)
                        (nf  (VSB8.mapAccumL mapAccumLF8 []) vb8)
                        (nf    (B8.mapAccumL mapAccumLF8 []) b8)
                        (nf  (VSBL.mapAccumL mapAccumLF  []) vbl)
                        (nf    (BL.mapAccumL mapAccumLF  []) bl)
                        (nf (VSBL8.mapAccumL mapAccumLF8 []) vbl8)
                        (nf   (BL8.mapAccumL mapAccumLF8 []) bl8)

    , boo "mapAccumR"   (nf   (VSB.mapAccumR mapAccumRF  []) vb)
                        (nf     (B.mapAccumR mapAccumRF  []) b)
                        (nf  (VSB8.mapAccumR mapAccumRF8 []) vb8) -- <<loop>> !?!?
                        (nf    (B8.mapAccumR mapAccumRF8 []) b8)  -- <<loop>> !?!?
                        (nf  (VSBL.mapAccumR mapAccumRF  []) vbl)
                        (nf    (BL.mapAccumR mapAccumRF  []) bl)
                        (nf (VSBL8.mapAccumR mapAccumRF8 []) vbl8)
                        (nf   (BL8.mapAccumR mapAccumRF8 []) bl8) -- <<loop>> !?!?

    , boo "replicate"   (nf   (VSB.replicate o)   1)
                        (nf     (B.replicate o)   1)
                        (nf  (VSB8.replicate o)   'a')
                        (nf    (B8.replicate o)   'a')
                        (nf  (VSBL.replicate o64) 1)
                        (nf    (BL.replicate o64) 1)
                        (nf (VSBL8.replicate o64) 'a')
                        (nf   (BL8.replicate o64) 'a')

    , boo "unfoldr"     (nf   (VSB.unfoldr unfoldrF)  0)
                        (nf     (B.unfoldr unfoldrF)  0)
                        (nf  (VSB8.unfoldr unfoldrF8) 0)
                        (nf    (B8.unfoldr unfoldrF8) 0)
                        (nf  (VSBL.unfoldr unfoldrF)  0)
                        (nf    (BL.unfoldr unfoldrF)  0)
                        (nf (VSBL8.unfoldr unfoldrF8) 0)
                        (nf   (BL8.unfoldr unfoldrF8) 0)

    , bla "unfoldrN"    (nf   (VSB.unfoldrN k unfoldrF)  0)
                        (nf     (B.unfoldrN k unfoldrF)  0)
                        (nf  (VSB8.unfoldrN k unfoldrF8) 0)
                        (nf    (B8.unfoldrN k unfoldrF8) 0)

    , boo "take"        (nf   (VSB.take t)   vb)
                        (nf     (B.take t)   b)
                        (nf  (VSB8.take t)   vb8)
                        (nf    (B8.take t)   b8)
                        (nf  (VSBL.take t64) vbl)
                        (nf    (BL.take t64) bl)
                        (nf (VSBL8.take t64) vbl8)
                        (nf   (BL8.take t64) bl8)

    , boo "drop"        (nf   (VSB.drop d)   vb)
                        (nf     (B.drop d)   b)
                        (nf  (VSB8.drop d)   vb8)
                        (nf    (B8.drop d)   b8)
                        (nf  (VSBL.drop d64) vbl)
                        (nf    (BL.drop d64) bl)
                        (nf (VSBL8.drop d64) vbl8)
                        (nf   (BL8.drop d64) bl8)

    , boo "splitAt"     (nf   (VSB.splitAt s)   vb)
                        (nf     (B.splitAt s)   b)
                        (nf  (VSB8.splitAt s)   vb8)
                        (nf    (B8.splitAt s)   b8)
                        (nf  (VSBL.splitAt s64) vbl)
                        (nf    (BL.splitAt s64) bl)
                        (nf (VSBL8.splitAt s64) vbl8)
                        (nf   (BL8.splitAt s64) bl8)

    , boo "takeWhile"   (nf   (VSB.takeWhile takeWhileF)  vb)
                        (nf     (B.takeWhile takeWhileF)  b)
                        (nf  (VSB8.takeWhile takeWhileF8) vb8) -- Suspiciously fast!
                        (nf    (B8.takeWhile takeWhileF8) b8)
                        (nf  (VSBL.takeWhile takeWhileF)  vbl)
                        (nf    (BL.takeWhile takeWhileF)  bl)
                        (nf (VSBL8.takeWhile takeWhileF8) vbl8)
                        (nf   (BL8.takeWhile takeWhileF8) bl8)

    , boo "dropWhile"   (nf   (VSB.dropWhile dropWhileF)  vb)
                        (nf     (B.dropWhile dropWhileF)  b)
                        (nf  (VSB8.dropWhile dropWhileF8) vb8) -- Suspiciously fast!
                        (nf    (B8.dropWhile dropWhileF8) b8)
                        (nf  (VSBL.dropWhile dropWhileF)  vbl)
                        (nf    (BL.dropWhile dropWhileF)  bl)
                        (nf (VSBL8.dropWhile dropWhileF8) vbl8)
                        (nf   (BL8.dropWhile dropWhileF8) bl8)
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
bar vb b = [ bench "vector-bytestring" vb
           , bench "bytestring"        b
           ]

--------------------------------------------------------------------------------
-- Function arguments
--------------------------------------------------------------------------------

mapF :: Word8 -> Word8
mapF x = x * x + x + 3 -- Just some random expression

mapF8 :: Char -> Char
mapF8 = w2c . mapF . c2w

foldlF :: Word8 -> Word8 -> Word8
foldlF x z = x * z - z -- Just some random expression

foldlF8 :: Char -> Char -> Char
foldlF8 x z = w2c $ foldlF (c2w x) (c2w z)

foldrF :: Word8 -> Word8 -> Word8
foldrF = foldlF

foldrF8 :: Char -> Char -> Char
foldrF8 = foldlF8

anyF :: Word8 -> Bool
anyF = (== 255)

anyF8 :: Char -> Bool
anyF8 = anyF . c2w

allF :: Word8 -> Bool
allF = (<= 255)       -- True for every Word8

allF8 :: Char -> Bool
allF8 = allF . c2w    -- True for every ASCII Char

scanlF :: Word8 -> Word8 -> Word8
scanlF = foldlF

scanlF8 :: Char -> Char -> Char
scanlF8 = foldlF8

scanrF :: Word8 -> Word8 -> Word8
scanrF = scanlF

scanrF8 :: Char -> Char -> Char
scanrF8 = scanlF8

mapAccumLF :: [Word8] -> Word8 -> ([Word8], Word8)
mapAccumLF acc x = (x:acc, mapF x)

mapAccumLF8 :: [Char] -> Char -> ([Char], Char)
mapAccumLF8 acc c = (c:acc, mapF8 c)

mapAccumRF :: [Word8] -> Word8 -> ([Word8], Word8)
mapAccumRF = mapAccumLF

mapAccumRF8 :: [Char] -> Char -> ([Char], Char)
mapAccumRF8 = mapAccumRF8

unfoldrF :: Int -> Maybe (Word8, Int)
unfoldrF 1000000 = Nothing
unfoldrF n       = Just (fromIntegral n, n+1)

unfoldrF8 :: Int -> Maybe (Char, Int)
unfoldrF8 1000000 = Nothing
unfoldrF8 n       = Just (w2c $ fromIntegral n, n+1)

takeWhileF :: Word8 -> Bool
takeWhileF = allF              -- take everything

takeWhileF8 :: Char -> Bool
takeWhileF8 = takeWhileF . c2w -- take everything

dropWhileF :: Word8 -> Bool
dropWhileF = takeWhileF        -- drop everything

dropWhileF8 :: Char -> Bool
dropWhileF8 = dropWhileF . c2w -- drop everything

--------------------------------------------------------------------------------
-- Orphaned NFData instances for legacy ByteStrings
--------------------------------------------------------------------------------

instance NFData B.ByteString

instance NFData BL.ByteString where
    rnf BLI.Empty = ()
    rnf (BLI.Chunk c cs) = rnf c `seq` rnf cs
