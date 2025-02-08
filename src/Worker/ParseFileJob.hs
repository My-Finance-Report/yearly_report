module Worker.ParseFileJob (asyncFileProcess, resetFileProcessingJob, resetAllFileProcessingJobs, resetFileProcessingJobBySource) where

import Control.Exception (throwIO)
import Control.Monad (forM_)
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.ConnectionPool (getConnectionPool)
import Database.Models
import Database.Persist hiding (get)
import Database.Persist.Postgresql (ConnectionPool, SqlPersistT, get, runSqlPool)

asyncFileProcess :: Entity User -> Key UploadedPdf -> Maybe (Key UploadConfiguration) -> IO ()
asyncFileProcess user pdfId configId = do
  now <- getCurrentTime
  pool <- getConnectionPool
  runSqlPool (enqueueFileProcessingJob user pdfId configId now) pool

enqueueFileProcessingJob :: Entity User -> Key UploadedPdf -> Maybe (Key UploadConfiguration) -> UTCTime -> SqlPersistT IO ()
enqueueFileProcessingJob user pdfId configId now = do
  existingJob <- selectFirst [ProcessFileJobPdfId ==. pdfId] []
  case existingJob of
    Just (Entity jobId job) -> do
      liftIO $ putStrLn $ "Resetting job for PDF ID: " ++ show pdfId
      resetFileProcessingJob user jobId
    Nothing -> do
      liftIO $ putStrLn $ "Enqueuing new job for PDF ID: " ++ show pdfId
      insert_
        ProcessFileJob
          { processFileJobStatus = Pending,
            processFileJobCreatedAt = now,
            processFileJobLastTriedAt = Nothing,
            processFileJobUserId = entityKey user,
            processFileJobPdfId = pdfId,
            processFileJobConfigId = configId,
            processFileJobAttemptCount = 0,
            processFileJobArchived = False
          }

resetFileProcessingJob :: (MonadUnliftIO m) => Entity User -> Key ProcessFileJob -> m ()
resetFileProcessingJob user jobId = do
  pool <- liftIO getConnectionPool
  now <- liftIO getCurrentTime
  runSqlPool (updateJobStatusIfAuthorized jobId now) pool
  where
    updateJobStatusIfAuthorized jobId now = do
      maybeJob <- get jobId
      case maybeJob of
        Nothing -> liftIO $ throwIO $ userError "Job not found!"
        Just job ->
          if processFileJobUserId job /= entityKey user
            then liftIO $ throwIO $ userError "Unauthorized: This job does not belong to the user!"
            else
              update
                jobId
                [ ProcessFileJobStatus =. Pending,
                  ProcessFileJobLastTriedAt =. Nothing,
                  ProcessFileJobAttemptCount =. 0
                ]

resetFileProcessingJobBySource :: (MonadUnliftIO m) => Entity User -> Key TransactionSource -> m ()
resetFileProcessingJobBySource user sourceId = do
  pool <- liftIO getConnectionPool
  now <- liftIO getCurrentTime
  runSqlPool (updateJobsForSource sourceId now) pool
  where
    updateJobsForSource srcId now = do
      -- Find all configurations associated with the given TransactionSource
      configs <- selectList [UploadConfigurationTransactionSourceId ==. srcId] []
      liftIO $ print configs

      -- Extract configuration IDs
      let configIds = map entityKey configs

      liftIO $ print (Prelude.length configIds)

      -- Find all jobs that belong to this user and are linked to the transaction source
      jobs <-
        selectList
          [ ProcessFileJobConfigId <-. map Just configIds,
            ProcessFileJobUserId ==. entityKey user
          ]
          []

      liftIO $ print (Prelude.length jobs)

      -- Reset all matching jobs
      forM_ jobs $ \(Entity jobId _) -> do
        update
          jobId
          [ ProcessFileJobStatus =. Pending,
            ProcessFileJobLastTriedAt =. Nothing,
            ProcessFileJobAttemptCount =. 0
          ]

      liftIO $ putStrLn $ "Reset " ++ show (length jobs) ++ " jobs for Transaction Source ID: " ++ show srcId

resetAllFileProcessingJobs :: (MonadUnliftIO m) => Entity User -> m ()
resetAllFileProcessingJobs user = do
  pool <- liftIO getConnectionPool
  now <- liftIO getCurrentTime
  runSqlPool (updateJobsForUser now) pool
  where
    updateJobsForUser now = do
      updateWhere
        [ProcessFileJobUserId ==. entityKey user, ProcessFileJobArchived ==. False]
        [ ProcessFileJobStatus =. Pending,
          ProcessFileJobLastTriedAt =. Nothing,
          ProcessFileJobAttemptCount =. 0
        ]
