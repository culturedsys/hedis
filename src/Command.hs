{-# LANGUAGE DuplicateRecordFields #-}
module Command(Command(..), CommandError(..), SetArgs(..), GetArgs(..), IncrArgs(..)) where
import Data.ByteString ( ByteString )

data SetArgs = SetArgs { cKey :: ByteString, csValue :: ByteString } deriving (Show, Eq)
newtype GetArgs = GetArgs { cKey :: ByteString } deriving (Show, Eq)
newtype IncrArgs = IncrArgs { cKey :: ByteString } deriving (Show, Eq)  

data Command = Set SetArgs
  | Get GetArgs
  | Incr IncrArgs
  deriving (Show, Eq)


data CommandError = CommandParseError deriving (Show, Eq)