{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HtmlGenerators.Admin (renderListPage, renderSingleEntityPage) where

import Auth (getCurrentUser)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import Data.Aeson.Key ()
import Data.Aeson.Types (FromJSON)
import Data.String (IsString (fromString))
import Data.Text (Text, isInfixOf, pack, unpack)
import qualified Data.Text.Lazy as TL
import Database.Models (User)
import Database.Persist
import Database.Persist.Postgresql (ConnectionPool, SqlBackend, runSqlPool)
import Database.Persist.Sql (SqlPersistT, fromSqlKey, toSqlKey)
import HtmlGenerators.Layout (renderPage)
import Network.HTTP.Types (status400, status404)
import Text.Blaze.Html (Html, (!))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

renderListPage ::
  ( PersistEntity a,
    PersistEntityBackend a ~ SqlBackend,
    ToBackendKey SqlBackend a,
    Show a
  ) =>
  [Entity a] ->
  Html
renderListPage entities = H.docTypeHtml $ do
  H.head $ do
    H.title "All Entities"
  H.body $ do
    H.h1 "All Entities"
    H.table $ do
      H.thead $ H.tr $ do
        H.th "Entity ID"
        H.th "Entity Value"
        H.th "Details Link"
      H.tbody $ mapM_ renderRow entities
  where
    renderRow :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend, ToBackendKey SqlBackend a, Show a) => Entity a -> Html
    renderRow entity = H.tr $ do
      H.td (H.toHtml (show (fromSqlKey (entityKey entity))))
      H.td (H.toHtml (show (entityVal entity)))
      H.td $
        H.a ! A.href (H.toValue ("/admin/" <> unEntityNameDB (getEntityDBName (entityDef entity)) <> "/" <> pack (show (fromSqlKey (entityKey entity))))) $
          "Details"

renderSingleEntityPage ::
  forall a.
  ( PersistEntity a,
    PersistEntityBackend a ~ SqlBackend,
    ToBackendKey SqlBackend a,
    Show a
  ) =>
  Text ->
  Entity a ->
  Html
renderSingleEntityPage entityName entity = H.docTypeHtml $ do
  let entityId = fromSqlKey (entityKey entity)
  let updateUrl = "/admin/" <> entityName <> "/" <> pack (show entityId)
  let deleteUrl = "/admin/" <> entityName <> "/" <> pack (show entityId)
  let entityMeta = entityDef (Nothing :: Maybe a)

  H.head $ do
    H.title $ "Details for " <> H.toHtml entityName
  H.body $ do
    H.h1 $ "Details for " <> H.toHtml entityName

    -- Display entity information
    H.p $ do
      H.strong "Entity ID: "
      H.toHtml (show entityId)

    -- Update Form
    H.h2 "Update Entity"
    H.form ! A.method "POST" ! A.action (H.toValue updateUrl) $ do
      mapM_ renderField (getEntityFields entityMeta)
      H.button ! A.type_ "submit" $ "Update"

    -- Delete Button
    H.h2 "Delete Entity"
    H.form ! A.method "POST" ! A.action (H.toValue deleteUrl) $ do
      H.input ! A.type_ "hidden" ! A.name "_method" ! A.value "DELETE"
      H.button ! A.type_ "submit" $ "Delete"

    -- Add New Entity
    H.h2 "Create New Entity"
    H.form ! A.method "POST" ! A.action (H.toValue ("/admin/" <> entityName)) $ do
      mapM_ renderField (getEntityFields entityMeta)
      H.button ! A.type_ "submit" $ "Create New"
  where
    renderField :: FieldDef -> Html
    renderField field = do
      let fieldName = fieldHaskell field
          fieldTypeText = show (fieldType field) -- Get type as a string
          fieldInputType = determineInputType fieldTypeText
      H.div $ do
        H.label ! A.for (H.toValue $ show fieldName) $ H.toHtml (unpack (fieldNameToText fieldName))
        H.input
          ! A.type_ (H.toValue fieldInputType)
          ! A.name (H.toValue (fieldNameToText fieldName))
          ! A.placeholder (H.toValue ("Enter " <> fieldNameToText fieldName))

    fieldNameToText = pack . show

    determineInputType :: String -> Text
    determineInputType fieldTypeText
      | "Int" `isInfixOf` pack fieldTypeText = "number"
      | "Text" `isInfixOf` pack fieldTypeText = "text"
      | "Double" `isInfixOf` pack fieldTypeText = "number"
      | "Bool" `isInfixOf` pack fieldTypeText = "checkbox"
      | "UTCTime" `isInfixOf` pack fieldTypeText = "datetime-local"
      | otherwise = "text"