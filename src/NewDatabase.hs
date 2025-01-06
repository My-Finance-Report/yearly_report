{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module NewDatabase (initializeDatabase,  addTransactionSource) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Sql
import Models (TransactionSource (..))
import ConnectionPool



getTransactionSource :: (MonadIO m) => Key TransactionSource -> m (Maybe TransactionSource)
getTransactionSource sourceId = liftIO $ do
  pool <- getConnectionPool
  runSqlPool (get sourceId) pool

addTransactionSource :: Text -> IO (Key TransactionSource)
addTransactionSource sourceName = do
  pool <- getConnectionPool
  runSqlPool (insert $ TransactionSource sourceName) pool
