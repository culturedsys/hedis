module Store (Store(..), empty, set, get) where

import Data.ByteString (ByteString)
import qualified Data.Map as M

type Key = ByteString

newtype Value = StringValue ByteString

newtype Entry = Entry {
  value :: Value
}

newtype Store = Store (M.Map Key Entry)

empty :: Store
empty = Store M.empty

set :: Store -> ByteString -> ByteString -> Store
set (Store m) k v =
  Store $ M.insert k (Entry (StringValue v)) m

get :: Store -> ByteString -> (Maybe ByteString, Store)
get (Store m) k =
  case M.lookup k m of
    Just (Entry (StringValue s)) -> (Just s, Store m)
    Nothing -> (Nothing, Store m)