{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException (SomeException), try)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Database.ConnectionPool
import Database.Models
import Database.Persist
import Database.Persist.Postgresql (ConnectionPool, SqlPersistT, createPostgresqlPool, runSqlPool)
import Parsers (processPdfFile)
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout)

getRequiredEnv :: String -> IO String
getRequiredEnv key = do
  maybeValue <- lookupEnv key
  case maybeValue of
    Just value -> return value
    Nothing -> ioError $ userError $ "Environment variable not set: " ++ key

pollInterval :: Int
pollInterval = 10 * 1000000

main :: IO ()
main = do
  openAiKey <- liftIO $ getRequiredEnv "OPENAI_API_KEY"
  dbKey <- liftIO $ getRequiredEnv "DATABASE_URL"
  initializePool
  pool <- getConnectionPool
  putStrLn "Starting worker-task..."
  forever $ do
    threadDelay (5 * 1000000)
    putStrLn "Checking for new jobs..."
    processNextJob pool

processNextJob :: ConnectionPool -> IO ()
processNextJob pool = do
  maybeJob <- runSqlPool fetchAndLockNextJob pool
  case maybeJob of
    Just (Entity jobId jobData) -> do
      putStrLn $ "Processing job: " ++ show jobId
      success <- tryJob jobData
      now <- getCurrentTime
      let newStatus = if success then Completed else Failed
      runSqlPool (update jobId [ProcessFileJobStatus =. newStatus, ProcessFileJobLastTriedAt =. Just now]) pool
      putStrLn $ "Job " ++ show jobId ++ " completed with status: " ++ show newStatus
    Nothing -> putStrLn "No jobs available."

fetchAndLockNextJob :: SqlPersistT IO (Maybe (Entity ProcessFileJob))
fetchAndLockNextJob = do
  maybeJob <- selectFirst [ProcessFileJobStatus ==. Pending] []
  case maybeJob of
    Just (Entity jobId jobData) -> do
      now <- liftIO getCurrentTime
      update jobId [ProcessFileJobStatus =. Processing, ProcessFileJobLastTriedAt =. Just now]
      return $ Just (Entity jobId jobData)
    Nothing -> return Nothing

tryJob :: ProcessFileJob -> IO Bool
tryJob job = do
  result <- try (runJob job) :: IO (Either SomeException ())
  case result of
    Left err -> do
      putStrLn $ "Job failed: " ++ show err
      return False
    Right _ -> return True

runJob :: ProcessFileJob -> IO ()
runJob job = do
  pool <- getConnectionPool
  -- Fetch the PDF record
  maybePdf <- runSqlPool (getEntity (processFileJobPdfId job)) pool
  case maybePdf of
    Nothing -> putStrLn "Error: PDF record not found!"
    Just pdf -> do
      -- Fetch the User record
      maybeUser <- runSqlPool (getEntity (processFileJobUserId job)) pool
      case maybeUser of
        Nothing -> putStrLn "Error: User record not found!"
        Just userEntity -> do
          -- Fetch the Upload Configuration
          maybeConfig <- runSqlPool (getEntity (processFileJobConfigId job)) pool
          case maybeConfig of
            Just config -> do
              processPdfFile (entityVal userEntity) (entityKey pdf) config False
            Nothing -> putStrLn "Error: Missing job configuration!"