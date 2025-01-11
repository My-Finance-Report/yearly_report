module Database.Category
  ( updateCategory,
    getCategory,
    getCategoriesBySource,
    addCategory,
    ensureCategoriesExist,
  )
where

import ConnectionPool
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (unliftIO))
import Data.Text (Text)
import Database.Persist (Entity (..), Filter)
import Database.Persist.Postgresql (insert, rawSql, runSqlPool, selectFirst, (==.))
import Database.Persist.Sql
import Models
import Types

getCategoriesBySource :: (MonadUnliftIO m) => Entity User -> Key TransactionSource -> m [Entity Category]
getCategoriesBySource user sourceId = do
  pool <- liftIO getConnectionPool
  runSqlPool queryCategories pool
  where
    queryCategories = selectList [CategorySourceId ==. sourceId] [Asc CategoryId]

getCategory :: (MonadUnliftIO m) => Entity User -> Key Category -> m (Entity Category, Entity TransactionSource)
getCategory user categoryId = do
  pool <- liftIO getConnectionPool
  runSqlPool queryCategory pool
  where
    queryCategory = do
      maybeCategory <- getEntity categoryId
      case maybeCategory of
        Nothing -> liftIO $ fail $ "No category found with id=" ++ show (fromSqlKey categoryId)
        Just categoryEntity -> do
          let sourceId = categorySourceId $ entityVal categoryEntity
          maybeSource <- getEntity sourceId
          case maybeSource of
            Nothing -> liftIO $ fail $ "No transaction source found for category id=" ++ show (fromSqlKey categoryId)
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
    queryUpdateCategory =
      update
        categoryId
        [ CategoryName =. newName
        ]

ensureCategoriesExist :: Entity User -> Key TransactionSource -> [Text] -> SqlPersistT IO ()
ensureCategoriesExist user sourceId categories = do
  forM_ categories $ \categoryName -> do
    existingCategory <-
      selectFirst
        [CategoryName ==. categoryName, CategorySourceId ==. sourceId]
        []
    case existingCategory of
      Nothing -> do
        _ <- insert $ Category categoryName sourceId (entityKey user)
        return ()
      Just _ -> return ()