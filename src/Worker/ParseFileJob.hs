module Worker.ParseFileJob (asyncFileUpload) where

import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.ConnectionPool (getConnectionPool)
import Database.Models
import Database.Persist hiding (get)
import Database.Persist.Postgresql (ConnectionPool, SqlPersistT, runSqlPool)

asyncFileUpload :: Entity User -> Key UploadedPdf -> Entity UploadConfiguration -> IO ()
asyncFileUpload user pdfId config = do
  now <- getCurrentTime
  pool <- getConnectionPool
  runSqlPool (enqueueFileProcessingJob user pdfId config now) pool

enqueueFileProcessingJob :: Entity User -> Key UploadedPdf -> Entity UploadConfiguration -> UTCTime -> SqlPersistT IO ()
enqueueFileProcessingJob user pdfId config now = do
  insert_
    ProcessFileJob
      { processFileJobStatus = Pending,
        processFileJobCreatedAt = now,
        processFileJobLastTriedAt = Nothing,
        processFileJobUserId = entityKey user,
        processFileJobPdfId = pdfId,
        processFileJobConfigId = entityKey config,
        processFileJobAttemptCount = 0
      }