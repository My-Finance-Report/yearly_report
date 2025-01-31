module Database.Jobs
  ( getFileJobsCount,
  )
where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (unliftIO))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Map (Map, fromList, toList)
import Data.Text (Text)
import Database.ConnectionPool
import Database.Models
import Database.Persist (Entity (..), Filter)
import Database.Persist.Postgresql (insert, rawSql, runSqlPool, selectFirst, (==.))
import Database.Persist.Sql
import Database.TransactionSource (getAllTransactionSources)
import Types

getFileJobsCount :: (MonadUnliftIO m) => Entity User -> JobStatus -> m (Int)
getFileJobsCount user status = do
  pool <- liftIO getConnectionPool
  jobList <- runSqlPool queryJobs pool
  return (length jobList)
  where
    queryJobs = selectList [ProcessFileJobUserId ==. entityKey user, ProcessFileJobStatus ==. status] []