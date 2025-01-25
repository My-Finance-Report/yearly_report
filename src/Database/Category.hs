module Database.Category
  ( updateCategory,
    getCategory,
    getCategoriesAndSources,
    getCategoriesBySource,
    addCategory,
    removeCategory,
    ensureCategoriesExist,
    ensureCategoryExists,
  )
where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (unliftIO))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text (Text)

import Data.Map (Map, toList, fromList)
import Database.ConnectionPool
import Database.Models
import Database.Persist (Entity (..), Filter)
import Database.Persist.Postgresql (insert, rawSql, runSqlPool, selectFirst, (==.))
import Database.Persist.Sql
import Types
import Database.TransactionSource (getAllTransactionSources)

getCategoriesBySource :: (MonadUnliftIO m) => Entity User -> Key TransactionSource -> m [Entity Category]
getCategoriesBySource user sourceId = do
  pool <- liftIO getConnectionPool
  runSqlPool queryCategories pool
  where
    queryCategories = selectList [CategorySourceId ==. sourceId, CategoryUserId ==. entityKey user, CategoryArchived ==. False] [Asc CategoryId]

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
  runSqlPool queryAddOrUnarchiveCategory pool
  where
    queryAddOrUnarchiveCategory = do
      maybeCategory <- getBy $ UniqueCategory categoryName sourceId
      case maybeCategory of
        Just (Entity categoryId category)
          | categoryArchived category -> do
              -- Unarchive the category
              update categoryId [CategoryArchived =. False]
              return categoryId
        _ -> do
          -- Insert a new category
          insert $ Category categoryName sourceId (entityKey user) False

removeCategory :: (MonadUnliftIO m) => Entity User -> Key Category -> m ()
removeCategory user categoryId = do
  pool <- liftIO getConnectionPool
  runSqlPool queryArchiveCategory pool
  where
    queryArchiveCategory = do
      maybeCategory <- get categoryId
      case maybeCategory of
        Nothing -> liftIO $ putStrLn $ "Category not found: " ++ show (fromSqlKey categoryId)
        Just category -> do
          if categoryUserId category /= entityKey user
            then liftIO $ putStrLn $ "Unauthorized attempt to archive category: " ++ show (fromSqlKey categoryId)
            else do
              update categoryId [CategoryArchived =. True]
              liftIO $ putStrLn $ "Category " ++ show (fromSqlKey categoryId) ++ " archived successfully."

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
        [CategoryName ==. categoryName, CategorySourceId ==. sourceId, CategoryUserId ==. entityKey user, CategoryArchived ==. False]
        []
    case existingCategory of
      Nothing -> insert_ $ Category categoryName sourceId (entityKey user) False
      Just _ -> return ()

ensureCategoryExists :: (MonadIO m) => Entity User -> Text -> Key TransactionSource -> ReaderT SqlBackend m (Key Category)
ensureCategoryExists user catName sourceId = do
  maybeCategory <-
    selectFirst
      [CategoryName ==. catName, CategorySourceId ==. sourceId, CategoryUserId ==. entityKey user, CategoryArchived ==. False]
      []
  case maybeCategory of
    Just (Entity categoryId _) -> return categoryId
    Nothing -> insert $ Category catName sourceId (entityKey user) False

getCategoriesAndSources :: Entity User -> IO (Map (Entity TransactionSource) [Entity Category])
getCategoriesAndSources user = do
  transactionSources <- getAllTransactionSources user
  categoriesBySource <- mapM (getCategoriesBySource user . entityKey) transactionSources
  return $ fromList $ zip transactionSources categoriesBySource

