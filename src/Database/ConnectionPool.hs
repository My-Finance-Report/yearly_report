{-# LANGUAGE OverloadedStrings #-}

module Database.ConnectionPool (initializePool, getConnectionPool, migratePostgres) where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text, unpack)
import Data.Time (getCurrentTime)
import Database.Models
import Database.Persist.Postgresql
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
          -- liftIO $ runSqlConn migrateEnsureProcessFileJobs backend
      )
      backend

{- migrateEnsureProcessFileJobs :: SqlPersistT IO ()
migrateEnsureProcessFileJobs = do
  now <- liftIO getCurrentTime
  -- Fetch all PDFs
  allPdfs <- selectList [] []

  liftIO $ print $ Prelude.length allPdfs

  forM_ allPdfs $ \(Entity pdfId pdf) -> do
    liftIO $ print $ pdfId

    existingJob <- selectFirst [ProcessFileJobPdfId ==. pdfId] []

    liftIO $ print $ "hot here"

    case existingJob of
      Just _ -> liftIO $ putStrLn $ "Skipping: Job already exists for PDF ID: " ++ show pdfId
      Nothing -> do
        liftIO $ putStrLn $ "Checking config for PDF ID: " ++ show pdfId
        maybeUploadConfig <- getUploadConfigurationFromPdf pdfId

        liftIO $ print $ "got here"

        case maybeUploadConfig of
          Just (Entity configId config) -> do
            liftIO $ putStrLn $ "Creating new job for PDF ID: " ++ show pdfId
            insert_
              ProcessFileJob
                { processFileJobStatus = Pending,
                  processFileJobCreatedAt = now,
                  processFileJobLastTriedAt = Nothing,
                  processFileJobUserId = uploadedPdfUserId pdf,
                  processFileJobPdfId = pdfId,
                  processFileJobConfigId = configId,
                  processFileJobAttemptCount = 0,
                  processFileJobArchived = False
                }
          Nothing -> liftIO $ putStrLn $ "Skipping PDF ID: " ++ show pdfId ++ " (No valid config)"

getUploadConfigurationFromPdf :: (MonadUnliftIO m) => Key UploadedPdf -> m (Maybe (Entity UploadConfiguration))
getUploadConfigurationFromPdf pdfId = do
  pool <- liftIO getConnectionPool
  runSqlPool queryUploadConfiguration pool
  where
    queryUploadConfiguration = do
      maybePdf <- get pdfId

      liftIO $ print "a pdf"
      case maybePdf of
        Nothing -> do
          liftIO $ print "no pdf"
          return Nothing
        Just pdf -> do
          let filename = uploadedPdfFilename pdf
          let rawText = uploadedPdfRawContent pdf
          liftIO $ print "got a pdf"

          liftIO $ print "Executing rawSql..."
          results <-
            rawSql
              "SELECT ?? FROM upload_configuration WHERE ? ~* filename_regex"
              [toPersistValue (filename <> rawText)]
          liftIO $ print "Finished rawSql execution"

          return $ listToMaybe results
 -}