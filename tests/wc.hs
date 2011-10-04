import System.Environment
import qualified Data.Vector.Storable.ByteString.Char8 as B
main = do
    n <- head `fmap` getArgs
    f <- B.readFile n
    print . B.count '\n' $ f

-- import qualified Data.Vector.Storable.ByteString.Lazy as L
-- main = print . L.count 10 =<< L.getContents

--
-- rule should rewrite this to:
-- main = print . length . B.lines =<< B.readFile "bigdata" -- B.getContents
--
