module BsUtil (intToByteString, word8ArrayToInt, byteStringToInt) where

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8 (pack)
import Data.Word8

intToByteString :: Int -> ByteString
intToByteString = C8.pack . show

word8ArrayToInt :: [Word8] -> Maybe Int
word8ArrayToInt =
  foldM
    ( \acc digit ->
        let value = fromIntegral (digit - _0)
         in if value < 0 || value > 9 then Nothing else Just $ acc * 10 + value
    )
    0

byteStringToInt :: ByteString -> Maybe Int
byteStringToInt = word8ArrayToInt . BS.unpack