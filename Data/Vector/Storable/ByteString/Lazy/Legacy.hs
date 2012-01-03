{-# LANGUAGE CPP, NoImplicitPrelude #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif

-- | Convert our lazy @'ByteString's@ to and from /legacy/ lazy
-- @'Legacy.ByteString's@ (from the @bytestring@ package).
module Data.Vector.Storable.ByteString.Lazy.Legacy where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Function ( (.) )

-- from bytestring:
import qualified Data.ByteString.Lazy.Internal as Legacy 
    ( ByteString(Empty, Chunk), foldrChunks )

-- from vector-bytestring (this package):
import Data.Vector.Storable.ByteString.Lazy.Internal 
    ( ByteString(Empty, Chunk), foldrChunks )

import qualified Data.Vector.Storable.ByteString.Legacy as VSB 
    ( toLegacyByteString, fromLegacyByteString )

--------------------------------------------------------------------------------
-- Legacy conversions
--------------------------------------------------------------------------------

-- | O(1) Convert our lazy 'ByteString' to a /legacy/ lazy 'Legacy.ByteString'
-- (from the @bytestring@ package).
toLegacyByteString :: ByteString -> Legacy.ByteString
toLegacyByteString = foldrChunks (Legacy.Chunk . VSB.toLegacyByteString) Legacy.Empty
{-# INLINE toLegacyByteString #-}

-- | O(1) Convert a /legacy/ lazy 'Legacy.ByteString' (from the @bytestring@ package)
-- to our lazy 'ByteString'.
fromLegacyByteString :: Legacy.ByteString -> ByteString
fromLegacyByteString = Legacy.foldrChunks (Chunk . VSB.fromLegacyByteString) Empty
{-# INLINE fromLegacyByteString #-}
