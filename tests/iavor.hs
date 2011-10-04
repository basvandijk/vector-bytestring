import qualified Data.Vector.Storable.ByteString as B
import Data.Vector.Storable.ByteString (packCString)
import Foreign.C.String
import Foreign

main = do x <- newCString "Hello"
          s <- packCString x
          let h1  = B.head s
          print s
          poke x (toEnum 97)
          print s
          let h2 = B.head s
          print h1
          print h2

{-

$ runhaskell iavor.hs
"Hello"
"Hello"
72
72

-}
