import Data.Bits

maxNbitsUsigned n = 2^n - 1
minNbitsSigned  n = 1 - 2^(n-1)
maxNbitsSigned  n = 2^(n-1)

getNthByte number nbyte = ( number  `shiftR` (nbyte*8))  .&. 255

splitNbytes number nbytes = map (getNthByte number) [0..nbytes]

bytes2num bytearray =  foldr (\a b -> a + (shiftL b 8)) 0 bytearray

num2bytes number = bytes ++ [getNthByte number (length bytes)]
    where
    bytes = last $ takeWhile (\n -> bytes2num  n < number) $ bytesSequences
    bytesSequences = map (\n -> take n (byteStream number)) [0..]
    byteStream number = map (getNthByte number) [0..]
    


    
    
