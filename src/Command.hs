module Command(Command(..), CommandError(..)) where
import Data.ByteString

data Command = Set { cKey :: ByteString, csValue :: ByteString }
  | Get { cKey :: ByteString }
  deriving (Show, Eq)


data CommandError = CommandParseError deriving (Show, Eq)