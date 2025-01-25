{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Configurations
  ( getFirstSankeyConfig,
    saveSankeyConfig,
    addSankeyLinkage,
    removeSankeyLinkage,
    addSankeyInput,
    removeSankeyInput,
  )
where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Database.Category
import Database.ConnectionPool (getConnectionPool)
import Database.Models
import Database.Persist (Entity (..), PersistEntity (Key), SelectOpt (Asc))
import Database.Persist.Postgresql
import Database.Persist.Sql (selectList)
import Types

getFirstSankeyConfig :: (MonadUnliftIO m) => Entity User -> m (FullSankeyConfig, Key SankeyConfig)
getFirstSankeyConfig user = do
  pool <- liftIO getConnectionPool
  liftIO $ runSqlPool queryFirstSankeyConfig pool
  where
    queryFirstSankeyConfig :: SqlPersistT IO (FullSankeyConfig, Key SankeyConfig)
    queryFirstSankeyConfig = do
      maybeConfig <- selectFirst [SankeyConfigUserId ==. entityKey user] []
      configId <-
        case maybeConfig of
          Just (Entity existingConfigId _) -> return existingConfigId
          Nothing -> do
            insert $ SankeyConfig "Auto-Generated Config" (entityKey user)

      inputEntities <- selectList [SankeyInputConfigId ==. configId] []

      linkageEntities <- selectList [SankeyLinkageConfigId ==. configId] []

      inputs <- fmap catMaybes $ forM inputEntities $ \(Entity _ (SankeyInput _ sourceId categoryId)) -> do
        maybeSource <- getEntity sourceId
        maybeCategory <- getEntity categoryId
        return $ (,) <$> maybeSource <*> maybeCategory

      linkages <- fmap catMaybes $ forM linkageEntities $ \(Entity _ (SankeyLinkage _ sourceId categoryId targetId)) -> do
        maybeSource <- getEntity sourceId
        maybeCategory <- getEntity categoryId
        maybeTarget <- getEntity targetId
        return $ (,,) <$> maybeSource <*> maybeCategory <*> maybeTarget

      return
        ( FullSankeyConfig
            { inputs = inputs,
              linkages = linkages
            },
          configId
        )

addSankeyInput :: Entity User -> Key SankeyConfig -> Key TransactionSource -> Key Category -> IO ()
addSankeyInput user configId sourceId categoryId = do
  pool <- getConnectionPool
  runSqlPool
    ( do
        -- Check if the SankeyConfig belongs to the user
        maybeConfig <- get configId
        case maybeConfig of
          Just SankeyConfig {sankeyConfigUserId}
            | sankeyConfigUserId == entityKey user ->
                insert_ $ SankeyInput configId sourceId categoryId
          _ -> error "Unauthorized access: Cannot modify this configuration"
    )
    pool

removeSankeyInput :: Entity User -> Key SankeyConfig -> Key TransactionSource -> Key Category -> IO ()
removeSankeyInput user configId sourceId categoryId = do
  pool <- getConnectionPool
  runSqlPool
    ( do
        -- Check if the SankeyConfig belongs to the user
        maybeConfig <- get configId
        case maybeConfig of
          Just SankeyConfig {sankeyConfigUserId}
            | sankeyConfigUserId == entityKey user ->
                deleteWhere
                  [ SankeyInputConfigId ==. configId,
                    SankeyInputSourceId ==. sourceId,
                    SankeyInputCategoryId ==. categoryId
                  ]
          _ -> error "Unauthorized access: Cannot delete this input"
    )
    pool

addSankeyLinkage :: Entity User -> Key SankeyConfig -> Key TransactionSource -> Key Category -> Key TransactionSource -> IO ()
addSankeyLinkage user configId sourceId categoryId targetSourceId = do
  pool <- getConnectionPool
  runSqlPool
    ( do
        -- Check if the SankeyConfig belongs to the user
        maybeConfig <- get configId
        case maybeConfig of
          Just SankeyConfig {sankeyConfigUserId}
            | sankeyConfigUserId == entityKey user -> insert_ $ SankeyLinkage configId sourceId categoryId targetSourceId
          _ -> error "Unauthorized access: Cannot modify this configuration"
    )
    pool

removeSankeyLinkage :: Entity User -> Key SankeyConfig -> Key TransactionSource -> Key Category -> Key TransactionSource -> IO ()
removeSankeyLinkage user configId sourceId categoryId targetSourceId = do
  pool <- getConnectionPool
  runSqlPool
    ( do
        -- Check if the SankeyConfig belongs to the user
        maybeConfig <- get configId
        case maybeConfig of
          Just SankeyConfig {sankeyConfigUserId}
            | sankeyConfigUserId == entityKey user ->
                deleteWhere
                  [ SankeyLinkageConfigId ==. configId,
                    SankeyLinkageSourceId ==. sourceId,
                    SankeyLinkageCategoryId ==. categoryId,
                    SankeyLinkageTargetSourceId ==. targetSourceId
                  ]
          _ -> error "Unauthorized access: Cannot delete this linkage"
    )
    pool

saveSankeyConfig :: (MonadUnliftIO m) => Entity User -> FullSankeyConfig -> m (Key SankeyConfig)
saveSankeyConfig user config = do
  pool <- liftIO getConnectionPool
  liftIO $ runSqlPool saveConfigQuery pool
  where
    saveConfigQuery :: SqlPersistT IO (Key SankeyConfig)
    saveConfigQuery = do
      sankeyConfigId <- upsertSankeyConfig

      -- Clear existing inputs and linkages
      deleteWhere [SankeyInputConfigId ==. sankeyConfigId]
      deleteWhere [SankeyLinkageConfigId ==. sankeyConfigId]

      -- Insert new inputs
      forM_ (inputs config) $ \(Entity sourceId _, Entity categoryId _) ->
        insert_ $ SankeyInput sankeyConfigId sourceId categoryId

      -- Insert new linkages (currently handles a single tuple, extendable to multiple)
      forM_ (linkages config) $ \(Entity sourceId _, Entity categoryId _, Entity targetSourceId _) -> do
        insert_ $ SankeyLinkage sankeyConfigId sourceId categoryId targetSourceId

      return sankeyConfigId

    -- Helper to upsert into `sankey_config`
    upsertSankeyConfig :: SqlPersistT IO (Key SankeyConfig)
    upsertSankeyConfig = do
      existing <-
        selectFirst
          [SankeyConfigUserId ==. entityKey user]
          []
      case existing of
        Just (Entity key _) -> do
          return key
        Nothing ->
          insert $
            SankeyConfig
              { sankeyConfigUserId = entityKey user,
                sankeyConfigName = "DEPRECATED" -- TODO remove from DB
              }
