{-
Counts the number of times each alphabetic character occurs in a dictionary.

Useful for benchmarking filter, map, sort and group.
-}
import Data.Char (isAlpha, toLower)
import Data.List (sortBy)
import qualified Data.Vector.Storable.ByteString.Char8 as P
import qualified Data.Vector.Storable.ByteString.Unsafe  as P
import qualified Data.Vector.Storable.ByteString.Internal  as P

main =
    mapM (\(a, b) -> putStrLn ([a] ++ ": " ++ show b))
    . sortBy (\a b -> snd b `compare` snd a)
    . map (\x -> (P.w2c . P.unsafeHead  $ x, P.length x))
    . P.group
    . P.sort
    . P.map toLower
    . P.filter isAlpha
    =<< P.getContents
