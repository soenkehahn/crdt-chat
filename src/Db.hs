
module Db where

import           Control.Concurrent
import           Data.Map
import           Data.Monoid
import           Prelude hiding (lookup)
import           System.Random

import           Api

type Db = MVar (Map ChatId Document)

newDb :: IO Db
newDb = newMVar $
  insert nilChatId mempty mempty

newChat :: Db -> IO ChatId
newChat mvar = modifyMVar mvar $ \ dict -> do
  chatId <- ChatId <$> randomIO
  return (insert chatId mempty dict, chatId)

sync :: Db -> ChatId -> Document -> IO (Maybe Document)
sync mvar chatId new = modifyMVar mvar $ \ dict ->
  case lookup chatId dict of
    Nothing -> return (dict, Nothing)
    Just old -> do
      let synced = old <> new
      return (insert chatId synced dict, Just synced)
