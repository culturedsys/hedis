module Command(Command(..), CommandError(..)) where
import Data.ByteString

data Command = Set { csKey :: ByteString, csValue :: ByteString }
  deriving (Show, Eq)


data CommandError = CommandParseError deriving (Show, Eq)