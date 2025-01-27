{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HtmlGenerators.Admin (renderListPage, renderSingleEntityPage) where

import Auth (getCurrentUser)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, encode)
import Data.Aeson.Key ()
import Data.Aeson.Types (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.String (IsString (fromString))
import Data.Text (Text, isInfixOf, pack, unpack)
import qualified Data.Text.Lazy as TL
import Database.Models (User)
import Database.Persist
import Database.Persist.EntityDef (EntityDef, getEntityFields)
import Database.Persist.Postgresql (ConnectionPool, SqlBackend, runSqlPool)
import Database.Persist.Sql (SqlPersistT, fromSqlKey, toSqlKey)
import HtmlGenerators.Layout (renderPage)
import Network.HTTP.Types (status400, status404)
import Text.Blaze.Html (Html, (!))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

renderListPage ::
  forall a.
  ( PersistEntity a,
    PersistEntityBackend a ~ SqlBackend,
    ToBackendKey SqlBackend a,
    Show a
  ) =>
  [Entity a] ->
  Html
renderListPage entities = H.div ! A.class_ "bg-gray-100 min-h-screen flex flex-col items-center p-6" $ do
  H.h1 ! A.class_ "text-3xl font-bold mb-6 text-gray-700" $ "All Entities"

  H.div ! A.class_ "w-full bg-white shadow-lg rounded-lg p-6 overflow-x-auto" $ do
    -- Only generate headers if we have entities
    case entities of
      (firstEntity : _) -> do
        let entityMeta = entityDef (Nothing :: Maybe a)
        let fieldDefs = getEntityFields entityMeta
        let headers = map (unFieldNameHS . fieldHaskell) fieldDefs

        -- Table Structure
        H.table ! A.class_ "w-full border-collapse border border-gray-300 shadow-md rounded-lg" $ do
          -- Table Head
          H.thead ! A.class_ "bg-gray-200 text-gray-700" $ H.tr $ do
            H.th ! A.class_ "border border-gray-300 px-4 py-2 text-left" $ "Entity ID"
            mapM_ (\field -> H.th ! A.class_ "border border-gray-300 px-4 py-2 text-left" $ H.toHtml field) headers
            H.th ! A.class_ "border border-gray-300 px-4 py-2 text-left" $ "Details"

          -- Table Body
          H.tbody $ mapM_ renderRow entities

      -- If no entities exist, show a message
      [] -> H.p ! A.class_ "text-gray-600" $ "No entities found."
  where
    renderRow :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend, ToBackendKey SqlBackend a, Show a) => Entity a -> Html
    renderRow entity = H.tr ! A.class_ "border-b border-gray-300 hover:bg-gray-100 transition-colors" $ do
      let entityId = fromSqlKey (entityKey entity)
      let fieldsVals = toPersistFields $ entityVal entity

      -- Entity ID Column
      H.td ! A.class_ "border border-gray-300 px-4 py-2 font-semibold" $ H.toHtml (show entityId)

      -- Field Values Columns
      mapM_ (\fieldVal -> H.td ! A.class_ "border border-gray-300 px-4 py-2 truncate max-w-xs" $ H.toHtml (persistValueToText fieldVal)) fieldsVals

      -- Details Link Column
      H.td ! A.class_ "border border-gray-300 px-4 py-2 text-blue-500 hover:underline" $
        H.a ! A.href (H.toValue ("/admin/" <> unEntityNameDB (getEntityDBName (entityDef entity)) <> "/" <> pack (show entityId))) $
          "Details"

    -- Convert PersistValue to Text
    persistValueToText :: PersistValue -> Text
    persistValueToText (PersistText t) = t
    persistValueToText (PersistInt64 n) = pack (show n)
    persistValueToText (PersistDouble d) = pack (show d)
    persistValueToText (PersistBool b) = if b then "✅" else "❌" -- Display booleans as checkmarks
    persistValueToText (PersistUTCTime t) = pack (show t)
    persistValueToText _ = "-"

renderSingleEntityPage ::
  forall a.
  ( PersistEntity a,
    PersistEntityBackend a ~ SqlBackend,
    ToBackendKey SqlBackend a,
    Show a,
    ToJSON a,
    FromJSON a
  ) =>
  Text ->
  Entity a ->
  Html
renderSingleEntityPage entityName entity = H.docTypeHtml $ do
  let entityId = fromSqlKey (entityKey entity)
  let updateUrl = "/admin/" <> entityName <> "/" <> pack (show entityId)
  let deleteUrl = "/admin/" <> entityName <> "/" <> pack (show entityId)
  let entityMeta = entityDef (Nothing :: Maybe a)
  let fieldsVals = toPersistFields $ entityVal entity
  let fieldsMeta = getEntityFields entityMeta
  let both = Prelude.zip fieldsMeta fieldsVals

  H.div ! A.class_ "bg-white shadow-lg rounded-lg p-6 w-full max-w-3xl" $ do
    H.h1 ! A.class_ "text-2xl font-bold mb-6 text-gray-700" $ "Edit " <> H.toHtml entityName

    -- Update Form
    H.form ! A.method "POST" ! A.action (H.toValue updateUrl) ! A.class_ "space-y-4" $ do
      mapM_ renderField both
      H.button
        ! A.type_ "submit"
        ! A.class_ "w-full bg-blue-500 hover:bg-blue-600 text-white py-3 rounded-lg shadow-md font-semibold"
        $ "Update"

    -- Delete Button
    H.h2 ! A.class_ "text-xl font-semibold mt-6 mb-3 text-red-600" $ "Delete"
    H.form ! A.method "POST" ! A.action (H.toValue deleteUrl) ! A.class_ "mt-3" $ do
      H.input ! A.type_ "hidden" ! A.name "_method" ! A.value "DELETE"
      H.button
        ! A.type_ "submit"
        ! A.class_ "w-full bg-red-500 hover:bg-red-600 text-white py-3 rounded-lg shadow-md font-semibold"
        $ "Delete"
  where
    -- Render a form field dynamically with its pre-filled value
    renderField :: (FieldDef, PersistValue) -> Html
    renderField (fullField, fieldVal) = do
      let fieldName = unFieldNameHS $ fieldHaskell fullField
          fieldTypeText = show (fieldType fullField)
          fieldInputType = determineInputType fieldTypeText
          fieldValue = persistValueToText fieldVal

      H.div ! A.class_ "flex flex-col space-y-2" $ do
        H.label
          ! A.for (H.toValue fieldName)
          ! A.class_ "text-gray-700 font-medium"
          $ H.toHtml fieldName

        if fieldInputType == "checkbox"
          then H.div ! A.class_ "flex items-center space-x-2" $ do
            H.input
              ! A.type_ "checkbox"
              ! A.name (H.toValue fieldName)
              ! (if fieldValue == "True" then A.checked "checked" else mempty)
              ! A.class_ "w-5 h-5 text-blue-500 border-gray-300 rounded focus:ring-2 focus:ring-blue-400"
            H.span ! A.class_ "text-gray-600" $ H.toHtml fieldName
          else
            H.input
              ! A.type_ (H.toValue fieldInputType)
              ! A.name (H.toValue fieldName)
              ! A.value (H.toValue fieldValue)
              ! A.class_ "border border-gray-300 rounded-lg px-4 py-2 focus:ring-2 focus:ring-blue-400 focus:border-blue-500"

-- Convert PersistValue to Text
persistValueToText :: PersistValue -> Text
persistValueToText (PersistText t) = t
persistValueToText (PersistInt64 n) = pack (show n)
persistValueToText (PersistDouble d) = pack (show d)
persistValueToText (PersistBool b) = pack (show b)
persistValueToText (PersistUTCTime t) = pack (show t)
persistValueToText _ = ""

-- Determine HTML input type based on the field type
determineInputType :: String -> Text
determineInputType fieldTypeText
  | "Int" `isInfixOf` pack fieldTypeText = "number"
  | "Text" `isInfixOf` pack fieldTypeText = "text"
  | "Double" `isInfixOf` pack fieldTypeText = "number"
  | "Bool" `isInfixOf` pack fieldTypeText = "checkbox"
  | "UTCTime" `isInfixOf` pack fieldTypeText = "datetime-local"
  | otherwise = "text"