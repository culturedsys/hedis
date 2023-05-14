module Store (Store(..), Error(..), Result, SetResult(..), empty, set, get, setNoOverwrite, incr, setWithExpiration) where

import Data.ByteString (ByteString)
import qualified Data.Map as M
import BsUtil (byteStringToInt, intToByteString)
import Data.Maybe (fromMaybe)
import Data.Time(UTCTime, NominalDiffTime, addUTCTime)


type Key = ByteString

data Value = StringValue ByteString
  | IntValue Int

data Entry = Entry {
  value :: Value,
  expiry :: Maybe UTCTime
}

data Error = BadType
  deriving (Eq, Show)

type Result a = Either Error (a, Store)

newtype Store = Store (M.Map Key Entry)

data SetResult = Modified | Unmodified deriving (Eq, Show)

data ExpiryChange = SetExpiry UTCTime | ClearExpiry | KeepExpiry

empty :: Store
empty = Store M.empty

lookupValue :: Key -> Store -> UTCTime -> Maybe Value
lookupValue k (Store m) now = do
  e <- M.lookup k m
  case expiry e of
    Nothing -> Just (value e)
    Just expiryTime -> if now < expiryTime then Just (value e) else Nothing

lookupString :: Key -> Store -> UTCTime -> Result (Maybe ByteString)
lookupString k store now = case lookupValue k store now of
  Nothing -> Right (Nothing, store)
  Just (IntValue i) -> Right  (Just $ intToByteString i, store)
  Just (StringValue s) -> Right (Just s, store)

lookupInt :: Key -> Store -> UTCTime -> Result (Maybe Int)
lookupInt k store now = case lookupValue k store now of
  Nothing -> Right (Nothing, store)
  Just v -> case v of
    (IntValue i) -> Right (Just i, store)
    (StringValue s) -> case byteStringToInt s of
      Nothing -> Left BadType
      Just i -> Right (Just i, store)

insertValue :: Key -> Value -> ExpiryChange -> Store -> Store
insertValue k v expiry (Store m) = Store $ M.alter (\ maybePrev -> Just $ case (maybePrev, expiry) of
    (Nothing, SetExpiry e) -> Entry { value = v, expiry = Just e}
    (Nothing, _) -> Entry { value = v, expiry = Nothing }
    (Just _, SetExpiry e) -> Entry { value = v, expiry = Just e}
    (Just _, ClearExpiry) -> Entry { value = v, expiry = Nothing}
    (Just prev, KeepExpiry) -> prev { value = v }
  ) k m

insertString :: Key -> ByteString -> ExpiryChange -> Store -> Store
insertString k v = insertValue k (StringValue v)

insertInt :: Key -> Int -> ExpiryChange -> Store -> Store
insertInt k v = insertValue k (IntValue v)

set :: Store -> ByteString -> ByteString -> UTCTime -> Result ()
set s k v _ = Right ((), insertString k v ClearExpiry s)

get :: Store -> ByteString -> UTCTime -> Result (Maybe ByteString)
get s k = lookupString k s

setNoOverwrite :: Store -> ByteString -> ByteString -> UTCTime -> Result SetResult
setNoOverwrite s k v now = case lookupString k s now of
  Left e -> Left e
  Right (Just _, s') -> Right (Unmodified, s')
  Right (Nothing, s') -> Right (Modified, insertString k v ClearExpiry s')

setWithExpiration :: Store -> ByteString -> ByteString -> NominalDiffTime -> UTCTime -> Result ()
setWithExpiration s k v d now = Right ((), insertString k v (SetExpiry (addUTCTime d now)) s)

incr :: Store -> ByteString -> UTCTime -> Result Int
incr s k now =
  case lookupInt k s now of
    Left e -> Left e
    Right (value, store) ->
      let res = (fromMaybe 0 value) + 1 in
        Right (res, insertInt k res KeepExpiry store)
