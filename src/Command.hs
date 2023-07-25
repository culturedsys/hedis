{-# LANGUAGE DuplicateRecordFields #-}
module Command(Command(..), CommandError(..), SetArgs(..), SetExArgs(..), GetArgs(..), IncrArgs(..), DecrArgs(..)) where
import Data.ByteString ( ByteString )
import Data.Time (NominalDiffTime)

data SetArgs = SetArgs { cKey :: ByteString, csValue :: ByteString } deriving (Show, Eq)
data SetExArgs = SetExArgs { cKey :: ByteString, cDuration :: NominalDiffTime, csValue :: ByteString } deriving (Show, Eq)
newtype GetArgs = GetArgs { cKey :: ByteString } deriving (Show, Eq)
newtype IncrArgs = IncrArgs { cKey :: ByteString } deriving (Show, Eq)  
newtype DecrArgs = DecrArgs { cKey :: ByteString } deriving (Show, Eq)

data Command = Set SetArgs
  | Get GetArgs
  | SetNx SetArgs
  | SetEx SetExArgs
  | Incr IncrArgs
  | Decr DecrArgs
  deriving (Show, Eq)


data CommandError = CommandParseError 
  | ArgParseError ByteString ByteString
  deriving (Show, Eq)