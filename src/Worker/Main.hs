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
      runSqlPool (update jobId [JobStatus =. newStatus, JobLastTriedAt =. Just now]) pool
      putStrLn $ "Job " ++ show jobId ++ " completed with status: " ++ show newStatus
    Nothing -> putStrLn "No jobs available."

fetchAndLockNextJob :: SqlPersistT IO (Maybe (Entity Job))
fetchAndLockNextJob = do
  maybeJob <- selectFirst [JobStatus ==. Pending] []
  case maybeJob of
    Just (Entity jobId jobData) -> do
      now <- liftIO getCurrentTime
      update jobId [JobStatus =. Processing, JobLastTriedAt =. Just now]
      return $ Just (Entity jobId jobData)
    Nothing -> return Nothing

tryJob :: Job -> IO Bool
tryJob job = do
  result <- try (runJob job) :: IO (Either SomeException ())
  case result of
    Left err -> do
      putStrLn $ "Job failed: " ++ show err
      return False
    Right _ -> return True

-- | Job execution logic based on job type
runJob :: Job -> IO ()
runJob job = case jobJobKind job of
  ParseTransactions -> putStrLn "Handling ParseTransactions..."
  CategorizeTransactions -> putStrLn "Handling CategorizeTransactions..."
  GenerateSankeyConfig -> putStrLn "Handling GenerateSankeyConfig..."