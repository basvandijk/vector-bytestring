import qualified Data.Vector.Storable.ByteString as B
import qualified Data.Vector.Storable.ByteString.Char8 as C
import System.IO
import Maybe

main = print . length . mapMaybe (sel . C.words) =<< B.hGetLines stdin
    where
        sel []     = Nothing
        sel (x:xs) = if x == (C.pack "The") then Just xs else Nothing
