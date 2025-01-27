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
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
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
import Text.Read (readMaybe)
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

  Web.Scotty.post (deleteRoute entityName) $ do
    (entityId :: Int) <- param "id"
    let entityKey :: Key a
        entityKey = toSqlKey (fromIntegral entityId)
    liftIO $ runSqlPool (Database.Persist.delete entityKey) pool
    text "Entity deleted successfully"

  Web.Scotty.post (updateRoute entityName) $ do
    (entityId :: Int) <- param "id"
    let entityKey :: Key a
        entityKey = toSqlKey (fromIntegral entityId)

    -- Get entity metadata
    let entityMeta = entityDef (Nothing :: Maybe a)
        fieldDefs = getEntityFields entityMeta

    -- Read form parameters dynamically
    fieldValues <- mapM extractParam fieldDefs

    case fromPersistValues fieldValues of
      Right updatedEntity -> do
        liftIO $ runSqlPool (replace entityKey updatedEntity) pool
        text "Entity updated successfully"
      Left err -> do
        liftIO $ print err
        status status400
        text "Failed to parse entity: "
  where
    -- Extract form parameters dynamically based on field names
    extractParam :: FieldDef -> ActionM PersistValue
    extractParam field = do
      let fieldName = unFieldNameHS $ fieldHaskell field
      paramText <- formParam (TL.fromStrict fieldName)
      return $ textToPersistValue paramText (fieldType field)

    -- Convert form text input to appropriate PersistValue
    textToPersistValue :: Text -> FieldType -> PersistValue
    textToPersistValue txt fieldType
      | "Id" `isInfixOf` pack (show fieldType) = PersistInt64 (readInt txt)
      | "Double" `isInfixOf` pack (show fieldType) = PersistDouble (readDouble txt)
      | "Bool" `isInfixOf` pack (show fieldType) = PersistBool (txt == "on" || txt == "true")
      | "UTCTime" `isInfixOf` pack (show fieldType) = PersistText "2025-01-27T14:57:00Z"
      | otherwise = PersistText txt

    -- Safe parsing helpers
    readInt :: Text -> Int64
    readInt txt = fromMaybe 0 (readMaybe (unpack txt) :: Maybe Int64)

    readDouble :: Text -> Double
    readDouble txt = fromMaybe 0.0 (readMaybe (unpack txt) :: Maybe Double)
