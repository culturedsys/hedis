module RespParseConduit (respParse) where

import Data.ByteString
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Resp (Resp(..))
import RespParser (parser)
import Data.Conduit.Attoparsec (conduitParserEither, ParseError)

respParse :: Monad m => ConduitT ByteString (Either ParseError Resp) m ()
respParse = conduitParserEither parser .| C.map (fmap snd)