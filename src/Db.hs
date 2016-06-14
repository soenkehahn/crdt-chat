
module Db where

import           Control.Concurrent
import           Data.Monoid

import           Api

type Db = MVar Document

newDb :: IO Db
newDb = newMVar mempty

sync :: Db -> Document -> IO Document
sync mvar new = modifyMVar mvar $ \ old -> do
  let synced = old <> new
  return (synced, synced)
