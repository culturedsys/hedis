{-# LANGUAGE OverloadedStrings #-}
module RespParseConduit (respParse, respWrite) where

import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import Data.Conduit ( ConduitT, yield, (.|), awaitForever )
import qualified Data.Conduit.Combinators as C
import Resp (Resp(..))
import RespParser (parser)
import Data.Conduit.Attoparsec (conduitParserEither, ParseError)
import Data.Word8
import Conduit (yieldMany)

respParse :: Monad m => ConduitT ByteString (Either ParseError Resp) m ()
respParse = conduitParserEither parser .| C.map (fmap snd)

respWrite :: Monad m => ConduitT Resp ByteString m ()
respWrite = awaitForever $ \r -> do
  case r of
    SimpleString s -> do
      yield "+"
      yield s
    Error e -> do
      yield "-"
      yield e
    Integer i -> do
      yield ":"
      yield $ intToByteString i
    BulkString s -> do
      yield "$"
      yield . intToByteString $ BS.length s
      yield "\r\n"
      yield s
    Array a -> do
      yield "*"
      yield . intToByteString $ length a
      yield "\r\n"
      yieldMany a .| respWrite
    NullString -> do
      yield "$-1"
  yield "\r\n"
  where 
    intToByteString =  BS.pack . go []
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