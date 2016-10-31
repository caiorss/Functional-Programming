{-
http://gbaz.github.io/slides/PracticalData-11-2012.pdf

-}
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)


type Field = ByteString
type Row = [Field]
type CSV = [Row]

parseCSV :: ByteString -> CSV
parseCSV string = ( map (B.split ',') . B.lines) string

(|>) x f = f x
(|>>) x f = map f x
(?>>) x f = filter f x

getFile file = B.readFile file

main :: IO ()
main = do
    contents <- B.readFile "Test.csv"
    print (parseCSV contents)




