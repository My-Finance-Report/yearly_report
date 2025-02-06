{-# LANGUAGE OverloadedStrings #-}

module Database.ConnectionPool (initializePool, getConnectionPool, migratePostgres) where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Crypto.Hash (Digest, MD5, hash)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Models
import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool, runSqlPool)
import Database.Persist.Postgresql hiding (get)
import Database.Persist.TH (mkMigrate)
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
    -- runSqlConn (runMigration migrateAll) backend
    runSqlConn
      ( do
          runMigration migrateAll
          liftIO $ runSqlConn migratePdfHashes backend
      )
      backend

computeMD5 :: Text -> Text
computeMD5 txt =
  let digest :: Digest MD5
      digest = hash (TE.encodeUtf8 txt)
   in T.pack (show digest)

migratePdfHashes :: SqlPersistT IO ()
migratePdfHashes = do
  pdfsWithoutHash <- selectList [UploadedPdfRawContentHash ==. Nothing] []

  liftIO $ putStrLn $ "Updating " ++ show (length pdfsWithoutHash) ++ " PDFs with hash"

  forM_ pdfsWithoutHash $ \(Entity pdfId pdf) -> do
    let newHash = Just (computeMD5 (uploadedPdfRawContent pdf))
    update pdfId [UploadedPdfRawContentHash =. newHash]

  liftIO $ putStrLn "MD5 hash migration completed!"
