{-# LANGUAGE OverloadedStrings #-}
module RespParseConduit (respParse, respWrite) where

import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import Data.Conduit ( ConduitT, yield, (.|), awaitForever )
import qualified Data.Conduit.Combinators as C
import Resp (Resp(..))
import RespParser (parser)
import Data.Conduit.Attoparsec (conduitParserEither, ParseError)
import Conduit (yieldMany)
import BsUtil (intToByteString)

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
