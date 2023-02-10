module BsUtil (intToByteString, word8ArrayToInt, byteStringToInt) where
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word8
import Control.Monad (foldM)

intToByteString :: Int -> ByteString
intToByteString =  BS.pack . go [] where
  go acc n = 
    let r = n `mod` 10
        rest = n `div` 10
        c = case r of
          0 -> _0
          1 -> _1
          2 -> _2
          3 -> _3
          4 -> _4
          5 -> _5
          6 -> _6
          7 -> _7
          8 -> _8
          9 -> _9 
          _ -> error " x `mod` 10 is somehow > 10" in
    if n < 10 then c : acc else go (c : acc) rest


word8ArrayToInt :: [Word8] -> Maybe Int
word8ArrayToInt = foldM (\acc digit -> 
  let value = fromIntegral (digit - _0) in
    if value < 0 || value > 9 then Nothing else Just $ acc * 10 + value) 0


byteStringToInt :: ByteString -> Maybe Int
byteStringToInt = word8ArrayToInt . BS.unpack