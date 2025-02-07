module Worker.ParseFileJob (asyncFileProcess, resetFileProcessingJob, resetAllFileProcessingJobs) where

import Control.Exception (throwIO)
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.ConnectionPool (getConnectionPool)
import Database.Models
import Database.Persist hiding (get)
import Database.Persist.Postgresql (ConnectionPool, SqlPersistT, get, runSqlPool)

asyncFileProcess :: Entity User -> Key UploadedPdf -> Entity UploadConfiguration -> IO ()
asyncFileProcess user pdfId config = do
  now <- getCurrentTime
  pool <- getConnectionPool
  runSqlPool (enqueueFileProcessingJob user pdfId config now) pool

enqueueFileProcessingJob :: Entity User -> Key UploadedPdf -> Entity UploadConfiguration -> UTCTime -> SqlPersistT IO ()
enqueueFileProcessingJob user pdfId config now = do
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
            processFileJobConfigId = entityKey config,
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
