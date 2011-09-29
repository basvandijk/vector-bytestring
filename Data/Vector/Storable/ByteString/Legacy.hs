{-# LANGUAGE NoImplicitPrelude #-}

module Data.Vector.Storable.ByteString.Legacy where

-- from vector:
import qualified Data.Vector.Storable as VS

-- from bytestring:
import qualified Data.ByteString.Internal as Legacy ( ByteString(PS) )

-- from vector-bytestring (this package):
import Data.Vector.Storable.ByteString.Internal ( ByteString )

toLegacyByteString :: ByteString -> Legacy.ByteString
toLegacyByteString v = Legacy.PS fp s l
    where
      (fp, s, l) = VS.unsafeToForeignPtr v

fromLegacyByteString :: Legacy.ByteString -> ByteString
fromLegacyByteString (Legacy.PS fp s l) = VS.unsafeFromForeignPtr fp s l
