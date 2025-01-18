{-# LANGUAGE OverloadedStrings #-}

module Database.Configurations
  ( getFirstSankeyConfig,
    saveSankeyConfig,
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

-- in theory the user can have more than one config, but for now only one really makes sense
getFirstSankeyConfig :: (MonadUnliftIO m) => Entity User -> m (Maybe FullSankeyConfig)
getFirstSankeyConfig user = do
  pool <- liftIO getConnectionPool
  liftIO $ runSqlPool queryFirstSankeyConfig pool
  where
    queryFirstSankeyConfig :: SqlPersistT IO (Maybe FullSankeyConfig)
    queryFirstSankeyConfig = do
      maybeConfig <- selectFirst [SankeyConfigUserId ==. entityKey user] []
      case maybeConfig of
        Nothing -> return Nothing
        Just (Entity configId SankeyConfig {sankeyConfigName = configName}) -> do
          -- Fetch the inputs for the config
          inputEntities <- selectList [SankeyInputConfigId ==. configId] []

          -- Fetch the linkages for the config
          linkageEntities <- selectList [SankeyLinkageConfigId ==. configId] []

          -- Resolve inputs into entities for TransactionSource and Category
          inputs <- fmap catMaybes $ forM inputEntities $ \(Entity _ (SankeyInput _ sourceId categoryId)) -> do
            maybeSource <- getEntity sourceId
            maybeCategory <- getEntity categoryId
            return $ (,) <$> maybeSource <*> maybeCategory

          -- Resolve the linkage into entities
          linkage <- case linkageEntities of
            [Entity _ (SankeyLinkage _ sourceId categoryId targetId)] -> do
              maybeSource <- getEntity sourceId
              maybeCategory <- getEntity categoryId
              maybeTarget <- getEntity targetId
              return $ (,,) <$> maybeSource <*> maybeCategory <*> maybeTarget
            _ -> return Nothing

          -- Construct and return the FullSankeyConfig if all components are valid
          case linkage of
            Just linkageTriple ->
              return $
                Just
                  FullSankeyConfig
                    { inputs = inputs,
                      linkages = linkageTriple
                    }
            Nothing -> return Nothing

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
      forM_ [linkages config] $ \(Entity sourceId _, Entity categoryId _, Entity targetSourceId _) -> do
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
