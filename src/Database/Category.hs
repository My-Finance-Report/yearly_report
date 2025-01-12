module Database.Category
  ( updateCategory,
    getCategory,
    getCategoriesBySource,
    addCategory,
    ensureCategoriesExist,
    ensureCategoryExists,
  )
where

import Database.ConnectionPool
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (unliftIO))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text (Text)
import Database.Persist (Entity (..), Filter)
import Database.Persist.Postgresql (insert, rawSql, runSqlPool, selectFirst, (==.))
import Database.Persist.Sql
import Database.Models
import Types

getCategoriesBySource :: (MonadUnliftIO m) => Entity User -> Key TransactionSource -> m [Entity Category]
getCategoriesBySource user sourceId = do
  pool <- liftIO getConnectionPool
  runSqlPool queryCategories pool
  where
    queryCategories = selectList [CategorySourceId ==. sourceId, CategoryUserId ==. entityKey user] [Asc CategoryId]

getCategory :: (MonadUnliftIO m) => Entity User -> Key Category -> m (Entity Category, Entity TransactionSource)
getCategory user categoryId = do
  pool <- liftIO getConnectionPool
  runSqlPool queryCategory pool
  where
    queryCategory = do
      maybeCategory <-
        selectFirst
          [CategoryId ==. categoryId, CategoryUserId ==. entityKey user]
          []
      case maybeCategory of
        Nothing -> liftIO $ fail $ "No category found with id=" ++ show (fromSqlKey categoryId) ++ " for user id=" ++ show (fromSqlKey $ entityKey user)
        Just categoryEntity -> do
          let sourceId = categorySourceId $ entityVal categoryEntity
          maybeSource <-
            selectFirst
              [TransactionSourceId ==. sourceId, TransactionSourceUserId ==. entityKey user]
              []
          case maybeSource of
            Nothing -> liftIO $ fail $ "No transaction source found for category id=" ++ show (fromSqlKey categoryId) ++ " and user id=" ++ show (fromSqlKey $ entityKey user)
            Just sourceEntity -> return (categoryEntity, sourceEntity)

addCategory :: (MonadUnliftIO m) => Entity User -> Text -> Key TransactionSource -> m (Key Category)
addCategory user categoryName sourceId = do
  pool <- liftIO getConnectionPool
  runSqlPool queryAddCategory pool
  where
    queryAddCategory = insert $ Category categoryName sourceId (entityKey user)

updateCategory :: (MonadUnliftIO m) => Entity User -> Key Category -> Text -> m ()
updateCategory user categoryId newName = do
  pool <- liftIO getConnectionPool
  runSqlPool queryUpdateCategory pool
  where
    queryUpdateCategory = do
      maybeCategory <-
        selectFirst
          [CategoryId ==. categoryId, CategoryUserId ==. entityKey user]
          []
      case maybeCategory of
        Nothing -> liftIO $ fail $ "No category found with id=" ++ show (fromSqlKey categoryId) ++ " for user id=" ++ show (fromSqlKey $ entityKey user)
        Just _ -> do
          update categoryId [CategoryName =. newName]

ensureCategoriesExist :: Entity User -> Key TransactionSource -> [Text] -> SqlPersistT IO ()
ensureCategoriesExist user sourceId categories = do
  forM_ categories $ \categoryName -> do
    existingCategory <-
      selectFirst
        [CategoryName ==. categoryName, CategorySourceId ==. sourceId, CategoryUserId ==. entityKey user]
        []
    case existingCategory of
      Nothing -> insert_ $ Category categoryName sourceId (entityKey user)
      Just _ -> return ()

ensureCategoryExists :: (MonadIO m) => Entity User -> Text -> Key TransactionSource -> ReaderT SqlBackend m (Key Category)
ensureCategoryExists user catName sourceId = do
  maybeCategory <-
    selectFirst
      [CategoryName ==. catName, CategorySourceId ==. sourceId, CategoryUserId ==. entityKey user]
      []
  case maybeCategory of
    Just (Entity categoryId _) -> return categoryId
    Nothing -> insert $ Category catName sourceId (entityKey user)
