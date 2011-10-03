{-# LANGUAGE NoImplicitPrelude #-}

-- | Convert our @'ByteString's@ to and from /legacy/ @'Legacy.ByteString's@
-- (from the @bytestring@ package).
module Data.Vector.Storable.ByteString.Legacy where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from vector:
import qualified Data.Vector.Storable as VS

-- from bytestring:
import qualified Data.ByteString.Internal as Legacy ( ByteString(PS) )

-- from vector-bytestring (this package):
import Data.Vector.Storable.ByteString.Internal ( ByteString )

--------------------------------------------------------------------------------
-- Legacy conversions
--------------------------------------------------------------------------------

-- | O(1) Convert our 'ByteString' to a /legacy/ 'Legacy.ByteString'
-- (from the @bytestring@ package).
toLegacyByteString :: ByteString -> Legacy.ByteString
toLegacyByteString v = Legacy.PS fp s l
    where
      (fp, s, l) = VS.unsafeToForeignPtr v
{-# INLINE toLegacyByteString #-}

-- | O(1) Convert a /legacy/ 'Legacy.ByteString' (from the @bytestring@ package)
-- to our 'ByteString'.
fromLegacyByteString :: Legacy.ByteString -> ByteString
fromLegacyByteString (Legacy.PS fp s l) = VS.unsafeFromForeignPtr fp s l
{-# INLINE fromLegacyByteString #-}
