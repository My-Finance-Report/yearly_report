{-# LANGUAGE OverloadedStrings #-}

module ConnectionPool (initializePool, getConnectionPool, migratePostgres) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStderrLoggingT)
-- Only for quick global variable usage
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool, runSqlPool)
import Database.Persist.Postgresql hiding (get)
import Models
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

connectionString :: IO ByteString
connectionString = do
  maybeDbUrl <- lookupEnv "DATABASE_URL"
  case maybeDbUrl of
    Just dbUrl -> do
      return $ pack dbUrl
    Nothing -> fail "Error: DATABASE_URL environment variable not set."

poolSize :: Int
poolSize = 10

{-# NOINLINE globalPool #-}
globalPool :: IORef (Maybe ConnectionPool)
globalPool = unsafePerformIO $ newIORef Nothing

initializePool :: IO ()
initializePool = do
  connStr <- connectionString
  pool <- runStderrLoggingT $ createPostgresqlPool connStr poolSize
  writeIORef globalPool (Just pool)

getConnectionPool :: IO ConnectionPool
getConnectionPool = do
  maybePool <- readIORef globalPool
  case maybePool of
    Just pool -> return pool
    Nothing -> error "Database connection pool not initialized. Call initializeDatabase first."

migratePostgres :: IO ()
migratePostgres = do
  connStr <- connectionString
  runStderrLoggingT $ withPostgresqlConn connStr $ \backend ->
    runSqlConn (runMigration migrateAll) backend