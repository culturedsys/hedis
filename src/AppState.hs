module AppState(AppState(..), newState) where
import Data.ByteString (ByteString)
import qualified Data.Map as M
import Control.Concurrent.STM (STM, TVar, newTVar)


data AppState = AppState {
  store :: TVar (M.Map ByteString ByteString)
}

newState :: STM AppState
newState = AppState <$> newTVar M.empty