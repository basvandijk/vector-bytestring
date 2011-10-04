import qualified Data.Vector.Storable.ByteString as P
import qualified Data.Vector.Storable.ByteString.Char8 as C

main = do
    a <- P.readFile "bigdata"
    b <- P.readFile "bigdata"
    print . P.length $ (P.pack (P.zipWith (const) a b)) -- should specialise
