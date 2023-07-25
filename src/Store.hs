{-# LANGUAGE LambdaCase #-}

module Store
  ( Store (..),
    StoreM,
    Error (..),
    Result,
    SetResult (..),
    empty,
    set,
    get,
    setNoOverwrite,
    incr,
    setWithExpiration,
    runStoreM,
    decr,
  )
where

import BsUtil (byteStringToInt, intToByteString)
import Control.Monad.Reader (MonadReader (ask), ReaderT, runReaderT)
import Control.Monad.State (runState)
import Control.Monad.State qualified as State
import Data.ByteString (ByteString)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)

type Key = ByteString

data Value
  = StringValue ByteString
  | IntValue Int

data Entry = Entry
  { value :: Value,
    expiry :: Maybe UTCTime
  }

data Error = BadType
  deriving (Eq, Show)

type Result a = Either Error a

newtype Store = Store (M.Map Key Entry)

type StoreM = ReaderT UTCTime (State.State Store)

data SetResult = Modified | Unmodified deriving (Eq, Show)

data ExpiryChange = SetExpiry NominalDiffTime | ClearExpiry | KeepExpiry

runStoreM :: Store -> UTCTime -> StoreM (Result a) -> (Result a, Store)
runStoreM store now m = runState (runReaderT m now) store

empty :: Store
empty = Store M.empty

lookupValue :: Key -> StoreM (Maybe Value)
lookupValue k = do
  (Store m) <- State.get
  now <- ask
  case M.lookup k m of
    Nothing -> return Nothing
    Just e -> case expiry e of
      Nothing -> return $ Just (value e)
      Just expiryTime ->
        if now < expiryTime
          then return $ Just (value e)
          else do
            State.put $ Store (M.delete k m)
            return Nothing

lookupType :: Key -> (Value -> Result a) -> StoreM (Result (Maybe a))
lookupType k converter = do
  value <- lookupValue k
  return $ case value of
    Nothing -> Right Nothing
    Just v -> Just <$> converter v

lookupString :: Key -> StoreM (Result (Maybe ByteString))
lookupString k = lookupType k $ \case
  IntValue i -> Right $ intToByteString i
  StringValue s -> Right s

lookupInt :: Key -> StoreM (Result (Maybe Int))
lookupInt k = lookupType k $ \case
  IntValue i -> Right i
  StringValue s -> case byteStringToInt s of
    Nothing -> Left BadType
    Just i -> Right i

insertValue :: Key -> Value -> ExpiryChange -> StoreM ()
insertValue k v expiryChange = do
  (Store m) <- State.get
  now <- ask
  let m' =
        M.alter
          ( \maybePrev -> Just $ case (expiryChange, maybePrev) of
              (KeepExpiry, Just prev) -> prev {value = v}
              (SetExpiry e, _) -> Entry {value = v, expiry = Just (addUTCTime e now)}
              (ClearExpiry, Just _) -> Entry {value = v, expiry = Nothing}
              (_, Nothing) -> Entry {value = v, expiry = Nothing}
          )
          k
          m
  State.put (Store m')

insertString :: Key -> ByteString -> ExpiryChange -> StoreM ()
insertString k v = insertValue k (StringValue v)

insertInt :: Key -> Int -> ExpiryChange -> StoreM ()
insertInt k v = insertValue k (IntValue v)

set :: ByteString -> ByteString -> StoreM (Result ())
set k v = do
  insertString k v ClearExpiry
  return $ Right ()

get :: ByteString -> StoreM (Result (Maybe ByteString))
get = lookupString

setNoOverwrite :: ByteString -> ByteString -> StoreM (Result SetResult)
setNoOverwrite k v = do
  existing <- lookupString k
  case existing of
    Left e -> return $ Left e
    Right (Just _) -> return $ Right Unmodified
    Right Nothing -> do
      insertString k v ClearExpiry
      return $ Right Modified

setWithExpiration :: ByteString -> ByteString -> NominalDiffTime -> StoreM (Result ())
setWithExpiration k v d = do
  insertString k v (SetExpiry d)
  return $ Right ()

incr :: ByteString -> StoreM (Result Int)
incr k = do
  existing <- lookupInt k
  case existing of
    Left e -> return $ Left e
    Right value ->
      let res = (fromMaybe 0 value) + 1
       in do
            insertInt k res KeepExpiry
            return $ Right res

decr :: ByteString -> StoreM (Result Int)
decr k = do
  existing <- lookupInt k
  case existing of
    Left e -> return $ Left e
    Right value ->
      let res = (fromMaybe 0 value) - 1
       in do
            insertInt k res KeepExpiry
            return $ Right res