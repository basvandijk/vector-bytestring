{-# LANGUAGE NoImplicitPrelude #-}

module Data.Vector.Storable.ByteString.Legacy where

-- from base:
import Foreign.ForeignPtr ( unsafeForeignPtrToPtr )
import Foreign.Ptr        ( minusPtr, plusPtr )

-- from vector:
import Data.Vector.Storable.Internal ( Vector(Vector) )

-- from bytestring:
import qualified Data.ByteString.Internal as Legacy ( ByteString(PS) )

-- from vector-bytestring (this package):
import Data.Vector.Storable.ByteString ( ByteString )

toLegacyByteString :: ByteString -> Legacy.ByteString
toLegacyByteString (Vector p l fp) =
    Legacy.PS fp (p `minusPtr` unsafeForeignPtrToPtr fp) l

fromLegacyByteString :: Legacy.ByteString -> ByteString
fromLegacyByteString (Legacy.PS fp s l) =
    Vector (unsafeForeignPtrToPtr fp `plusPtr` s) l fp
