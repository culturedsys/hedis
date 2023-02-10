module Store (Store(..), Error(..), Result, empty, set, get, incr) where

import Data.ByteString (ByteString)
import qualified Data.Map as M
import BsUtil (byteStringToInt, intToByteString)
import Data.Maybe (fromMaybe)


type Key = ByteString

data Value = StringValue ByteString
  | IntValue Int

newtype Entry = Entry {
  value :: Value
}

data Error = BadType 
  deriving (Eq, Show)

type Result a = Either Error (a, Store)

newtype Store = Store (M.Map Key Entry)

empty :: Store
empty = Store M.empty

lookupString :: Key -> Store -> Result (Maybe ByteString)
lookupString k store@(Store m) = case M.lookup k m of
  Nothing -> Right (Nothing, store)
  Just (Entry (IntValue i)) -> Right  (Just $ intToByteString i, store) 
  Just (Entry (StringValue s)) -> Right (Just s, store)

lookupInt :: Key -> Store -> Result (Maybe Int)
lookupInt k store@(Store m) = case M.lookup k m of
  Nothing -> Right (Nothing, store)
  Just e -> case value e of
    (IntValue i) -> Right (Just i, store)
    (StringValue s) -> case byteStringToInt s of
      Nothing -> Left BadType
      Just i -> Right (Just i, store)

insertString :: Key -> ByteString -> Store -> Store
insertString k v (Store m) =
  Store $ M.insert k (Entry (StringValue v)) m

insertInt :: Key -> Int -> Store -> Store
insertInt k v (Store m) = 
  Store $ M.insert k (Entry (IntValue v)) m

set :: Store -> ByteString -> ByteString -> Result ()
set s k v = Right ((), insertString k v s)

get :: Store -> ByteString -> Result (Maybe ByteString)
get s k = lookupString k s

incr :: Store -> ByteString -> Result Int
incr s k =
  case lookupInt k s of
    Left e -> Left e
    Right (value, store) ->
      let res = (fromMaybe 0 value) + 1 in
        Right (res, insertInt k res store) 
