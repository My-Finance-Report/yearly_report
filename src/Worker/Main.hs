{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException (SomeException), throw, throwIO, try)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
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
    putStrLn "Checking for new jobs..."
    processNextJob pool
    runSqlPool resetStuckJobs pool
    threadDelay pollInterval

-- this should not reset the tries, otherwise we run this risk of infinite loop to open-ai...
-- we should backstop on our budget, but thats not ideal
resetStuckJobs :: SqlPersistT IO ()
resetStuckJobs = do
  now <- liftIO getCurrentTime
  let timeout = 60 * 3 -- 3 minutes timeout
  liftIO $ putStrLn "resetting stuck jobs"
  updateWhere
    [ ProcessFileJobStatus ==. Processing,
      ProcessFileJobLastTriedAt <. Just (addUTCTime (negate timeout) now)
    ]
    [ProcessFileJobStatus =. Pending]

processNextJob :: ConnectionPool -> IO ()
processNextJob pool = do
  maybeJob <- runSqlPool fetchAndLockNextJob pool
  case maybeJob of
    Just (Entity jobId jobData) -> do
      putStrLn $ "Processing job: " ++ show jobId
      success <- tryJob (Entity jobId jobData) 
      now <- getCurrentTime
      let newStatus = if success then Completed else Failed
      runSqlPool (update jobId [ProcessFileJobStatus =. newStatus, ProcessFileJobLastTriedAt =. Just now]) pool
      putStrLn $ "Job " ++ show jobId ++ " completed with status: " ++ show newStatus
    Nothing -> putStrLn "No jobs available."

maxAttempts :: Int
maxAttempts = 5

-- This code will all need to become generic, but for now this is probably acceptable
fetchAndLockNextJob :: SqlPersistT IO (Maybe (Entity ProcessFileJob))
fetchAndLockNextJob = do
  maybeJob <-
    selectFirst
      [ ProcessFileJobStatus ==. Pending,
        ProcessFileJobAttemptCount <. maxAttempts,
        ProcessFileJobArchived ==. False
      ]
      []
  case maybeJob of
    Just (Entity jobId jobData) -> do
      now <- liftIO getCurrentTime
      update
        jobId
        [ ProcessFileJobStatus =. Processing,
          ProcessFileJobLastTriedAt =. Just now,
          ProcessFileJobAttemptCount +=. 1
        ]
      return $ Just (Entity jobId jobData)
    Nothing -> return Nothing

tryJob :: Entity ProcessFileJob -> IO Bool
tryJob job = do
  result <- try (runJob job) :: IO (Either SomeException ())
  case result of
    Left err -> do
      putStrLn $ "Job failed: " ++ show err
      return False
    Right _ -> return True

runJob :: Entity ProcessFileJob -> IO ()
runJob entityJob = do

  let job = entityVal entityJob

  pool <- getConnectionPool

  maybePdf <- runSqlPool (getEntity (processFileJobPdfId job)) pool
  pdf <- case maybePdf of
    Nothing -> throwIO $ userError $ unpack "PDF record not found!"
    Just entity -> return entity

  maybeUser <- runSqlPool (getEntity (processFileJobUserId job)) pool
  userEntity <- case maybeUser of
    Nothing -> throwIO $ userError $ unpack "User record not found!"
    Just entity -> return entity

  result <- processPdfFile userEntity  (entityKey entityJob) (entityKey pdf) (processFileJobConfigId job)

  case result of
    Just errorMsg -> throwIO $ userError $ unpack errorMsg
    Nothing -> return ()