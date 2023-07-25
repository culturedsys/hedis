module AppState (AppState (..), newState) where

import Control.Concurrent.STM (STM, TVar, newTVar)
import Store (Store, empty)

newtype AppState = AppState
  { store :: TVar Store
  }

newState :: STM AppState
newState = AppState <$> newTVar Store.empty