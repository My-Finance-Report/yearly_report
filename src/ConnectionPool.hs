{-# LANGUAGE OverloadedStrings #-}

module ConnectionPool (initializeDatabase, getConnectionPool, migratePostgres) where 
import Database.Persist.Postgresql hiding (get)
import Models
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool, runSqlPool)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO) -- Only for quick global variable usage
import Data.ByteString (ByteString)

-- Connection string and pool size
connectionString :: ByteString
connectionString = "host=localhost port=5433 user=persistent_user dbname=persistent_db password=persistent_pass"

poolSize :: Int
poolSize = 10

{-# NOINLINE globalPool #-}
globalPool :: IORef (Maybe ConnectionPool)
globalPool = unsafePerformIO $ newIORef Nothing

initializeDatabase :: IO ()
initializeDatabase = do
  pool <- runStderrLoggingT $ createPostgresqlPool connectionString poolSize
  writeIORef globalPool (Just pool)

getConnectionPool :: IO ConnectionPool
getConnectionPool = do
  maybePool <- readIORef globalPool
  case maybePool of
    Just pool -> return pool
    Nothing -> error "Database connection pool not initialized. Call initializeDatabase first."

migratePostgres :: IO ()
migratePostgres = runStderrLoggingT $ withPostgresqlConn connectionString $ \backend ->
  runSqlConn (runMigration migrateAll) backend

