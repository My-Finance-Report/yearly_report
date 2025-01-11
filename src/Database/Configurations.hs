module Database.Configurations (
    getFirstSankeyConfig,
    saveSankeyConfig,
) where


import Control.Monad (forM, forM_)
import ConnectionPool (getConnectionPool)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text (Text, unpack)
import Data.Maybe (catMaybes)
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Database.Category
import Database.Persist (Entity (..), PersistEntity (Key), SelectOpt (Asc))
import Database.Persist.Postgresql
import Database.Persist.Sql (selectList)
import Models
import Types



-- in theory the user can have more than one config, but for now only one really makes sense
getFirstSankeyConfig :: (MonadUnliftIO m) => m (Maybe FullSankeyConfig)
getFirstSankeyConfig = do
  pool <- liftIO getConnectionPool
  liftIO $ runSqlPool queryFirstSankeyConfig pool
  where
    queryFirstSankeyConfig :: SqlPersistT IO (Maybe FullSankeyConfig)
    queryFirstSankeyConfig = do
      -- Fetch the first SankeyConfig
      maybeConfig <- selectFirst [] []
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
                      linkages = linkageTriple,
                      mapKeyFunction = transactionSourceName . entityVal,
                      configName = configName
                    }
            Nothing -> return Nothing



saveSankeyConfig :: (MonadUnliftIO m) => FullSankeyConfig -> m (Key SankeyConfig)
saveSankeyConfig config = do
  pool <- liftIO getConnectionPool
  liftIO $ runSqlPool saveConfigQuery pool
  where
    saveConfigQuery :: SqlPersistT IO (Key SankeyConfig)
    saveConfigQuery = do
      -- Upsert into sankey_config
      sankeyConfigId <- upsertSankeyConfig

      -- Clear existing inputs and linkages
      deleteWhere [SankeyInputConfigId ==. sankeyConfigId]
      deleteWhere [SankeyLinkageConfigId ==. sankeyConfigId]

      -- Insert new inputs
      forM_ (inputs config) $ \(Entity sourceId _, Entity categoryId _) ->
        insert_ $ SankeyInput sankeyConfigId sourceId categoryId

      -- Insert new linkage
      let (Entity sourceId _, Entity categoryId _, Entity targetSourceId _) = linkages config
      insert_ $ SankeyLinkage sankeyConfigId sourceId categoryId targetSourceId

      return sankeyConfigId

    -- Helper to upsert into `sankey_config`
    upsertSankeyConfig :: SqlPersistT IO (Key SankeyConfig)
    upsertSankeyConfig = do
      existing <- selectFirst [SankeyConfigName ==. configName config] []
      case existing of
        Just (Entity key _) -> do
          update key [SankeyConfigName =. configName config]
          return key
        Nothing -> insert $ SankeyConfig (configName config)