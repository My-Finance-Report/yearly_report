{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.Admin.RegisterAdmin
  ( registerEntityRoutes,
  )
where

import Auth (getCurrentUser)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import Data.Aeson.Key ()
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.String (IsString (fromString))
import Data.Text (Text, isInfixOf, pack, unpack)
import qualified Data.Text.Lazy as TL
import Database.Models (User)
import Database.Persist
import Database.Persist.Postgresql (ConnectionPool, SqlBackend, runSqlPool)
import Database.Persist.Sql (SqlPersistT, fromSqlKey, toSqlKey)
import HtmlGenerators.Admin (renderListPage, renderSingleEntityPage)
import HtmlGenerators.Layout (renderPage)
import Network.HTTP.Types (status400, status404)
import Web.Scotty

routeWithNoId entityName =
  fromString ("/admin/" <> entityName)

routeWithId entityName =
  fromString ("/admin/" <> entityName <> "/:id")

deleteRoute entityName =
  fromString ("/admin/" <> entityName <> "/:id/delete")

updateRoute entityName =
  fromString ("/admin/" <> entityName <> "/:id/update")

registerEntityRoutes ::
  forall a.
  ( PersistEntity a,
    PersistEntityBackend a ~ SqlBackend,
    ToBackendKey SqlBackend a,
    Show a,
    SafeToInsert a,
    FromJSON a,
    ToJSON a
  ) =>
  String ->
  ConnectionPool ->
  ScottyM ()
registerEntityRoutes entityName pool = do
  Web.Scotty.get (routeWithNoId entityName) $ do
    currentUser <- getCurrentUser pool
    entities :: [Entity a] <-
      liftIO $ runSqlPool (selectList [] []) pool
    let content = renderListPage entities
    html $ renderPage currentUser "Admin" content False

  Web.Scotty.get (routeWithId entityName) $ do
    (entityId :: Int) <- param "id"
    currentUser <- getCurrentUser pool
    let entityKey :: Key a
        entityKey = toSqlKey (fromIntegral entityId)

    maybeEntity <-
      liftIO $
        runSqlPool (Database.Persist.getEntity entityKey) pool
    case maybeEntity of
      Just entity -> html $ renderPage currentUser "Admin" (renderSingleEntityPage (pack entityName) entity) False
      Nothing -> status status404

  Web.Scotty.post (routeWithNoId entityName) $ do
    bodyData <- body
    case decode bodyData of
      Just (entity :: a) -> do
        key <- liftIO $ runSqlPool (insert entity) pool
        json key
      Nothing -> status status400

  Web.Scotty.post (updateRoute entityName) $ do
    (entityId :: Int) <- param "id"
    let entityKey :: Key a
        entityKey = toSqlKey (fromIntegral entityId)
    bodyData <- body
    case decode bodyData of
      Just (entity :: a) -> do
        liftIO $ runSqlPool (replace entityKey entity) pool
        text "Entity updated successfully"
      Nothing -> status status400

  Web.Scotty.post (deleteRoute entityName) $ do
    (entityId :: Int) <- param "id"
    let entityKey :: Key a
        entityKey = toSqlKey (fromIntegral entityId)
    liftIO $ runSqlPool (Database.Persist.delete entityKey) pool
    text "Entity deleted successfully"
