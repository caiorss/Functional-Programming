{-
 -
 -  Tested on GHC 7.6.3
 -  
 -      http://tenminutetutor.com/base64-encoding 
 -      http://www.herongyang.com/encoding/Base64-Encoding-Algorithm.html  
        http://web.archive.org/web/20131125120025/http://www.apps.ietf.org/rfc/rfc4648.html-
-}

import Data.List.Split (chunksOf)
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.Word (Word8)
import Numeric (showHex)
import Data.Char (ord, chr)
import Data.List (findIndex)
import Data.Maybe (fromJust)

(|>) x f =  f x

{- Convert number to hexadecimal string -}
num2hex n =  showHex n ""

{- Converts a sequence of bytes (little endian) to a number 
 -
 -  
 -  >>> num2hex [0x12, 0x34] 
 -      0x3412
 -
 - -}
bytes2numL bytes =  foldr (\b0 b1 -> (shiftL b1 8) + b0) 0 bytes


{- Converts a sequece of bytes (big endian) to a number 
 - 
 -
 - >>> num2hex [0x12, 0x34]
 -      0x1234
 -
 --}
bytes2numB bytes = bytes2numL (reverse bytes)

 --word8TOint :: word8 -> Int
word8TOint w = fromIntegral w :: Int 

padBytes pad n bytes = 
    bytes ++ replicate (n - length bytes) pad
    
chunksOfbits n bytes = 
    map ((.&.) (2^n -1))  . takeWhile ((/=) 0) . iterate (\x -> shiftR x n) $ bytes 


base64alphabet1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
base64alphabet2 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
encode xs = map (\i -> base64alphabet2 !! i) xs


decodeCh xs = map  (fromJust . (\x -> findIndex ((==) x)  base64alphabet2)) xs
--decodeChar xs = 

b64encode = 
    encode 
    .  concat  
    .  map ((reverse . chunksOfbits 6)  .  bytes2numB   .  (padBytes 0x00 3))
    .  chunksOf 3  
    .  map word8TOint

drop_last n xs = take (length xs - n) xs 


b64enc bytes =     
    let chunks = chunksOf 3 bytes in
    let npads = 3 - length (last chunks) in
    let padded = (init chunks) ++ [(last chunks) ++ replicate npads 0x00] in 
    ((\x ->  x ++ (replicate npads '=' )) 
    .  encode 
    . drop_last npads 
    . concat  
    . map ( reverse  . (chunksOfbits 6) .  bytes2numB ))  padded


strToBase64 str = b64enc (map ord str)


bytes1 = [0x12, 0x34, 0x56, 0x78, 0x9A] 


secret = "aGVsbG8gd29ybGQgSGFza2VsbCAtIEhhc2tlbGwgaXMgYW1hemluZw=="
msg = "hello world Haskell - Haskell is amazing" 
