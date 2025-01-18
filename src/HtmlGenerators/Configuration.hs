{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.Configuration (renderConfigurationPage) where

import Control.Monad (forM_)
import Data.Map hiding ((!), (!?))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Database.Models
import Database.Persist
import Database.Persist.Postgresql (fromSqlKey)
import HtmlGenerators.OnboardingOne
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Types

renderUploadConfigurationsPage :: [Entity UploadConfiguration] -> [Entity TransactionSource] -> Html
renderUploadConfigurationsPage configurations transactionSources =
  H.div $ do
    H.h1 "Upload Configurations"
    H.table ! A.class_ "upload-configurations" $ do
      H.tr $ do
        H.th "Filename Pattern"
        H.th "Start Keyword"
        H.th "End Keyword"
        H.th "Transaction Source"
        H.th ! A.colspan "2" $ "Actions" -- Span two columns
      forM_ configurations $ \(Entity configId config) -> do
        H.tr $ do
          H.form
            ! A.method "post"
            ! A.action (toValue $ "/edit-upload-config/" <> show (fromSqlKey configId))
            $ do
              H.td $ H.input ! A.type_ "text" ! A.name "filenameRegex" ! A.value (toValue $ fromMaybe "" (uploadConfigurationFilenameRegex config))
              H.td $ H.input ! A.type_ "text" ! A.name "startKeyword" ! A.value (toValue $ fromMaybe "" (uploadConfigurationStartKeyword config))
              H.td $ H.input ! A.type_ "text" ! A.name "endKeyword" ! A.value (toValue $ fromMaybe "" (uploadConfigurationEndKeyword config))
              H.td
                $ H.select
                  ! A.name "transactionSourceId"
                $ do
                  forM_ transactionSources $ \(Entity tsId ts) -> do
                    H.option
                      ! A.value (toValue $ fromSqlKey tsId)
                      !? (uploadConfigurationTransactionSourceId config == tsId, A.selected "selected")
                      $ toHtml (transactionSourceName ts)
              H.td $ do
                H.input ! A.type_ "hidden" ! A.name "id" ! A.value (toValue $ fromSqlKey configId)
                H.input ! A.type_ "submit" ! A.value "Save"

          H.form
            ! A.method "post"
            ! A.action (toValue $ "/merge-upload-config/" <> show (fromSqlKey configId))
            $ H.td
            $ H.input ! A.type_ "submit" ! A.value "Merge"

    H.h2 "Add New Upload Configuration"
    H.form
      ! A.method "post"
      ! A.action "/add-upload-config"
      $ do
        H.label ! A.for "filenameRegex" $ "Filename Pattern:"
        H.input
          ! A.type_ "text"
          ! A.name "filenameRegex"
          ! A.id "filenameRegex"
          ! A.placeholder "Enter filename pattern"
        H.br

        H.label ! A.for "startKeyword" $ "Start Keyword:"
        H.input
          ! A.type_ "text"
          ! A.name "startKeyword"
          ! A.id "startKeyword"
          ! A.placeholder "Enter start keyword"
        H.br

        H.label ! A.for "endKeyword" $ "End Keyword:"
        H.input
          ! A.type_ "text"
          ! A.name "endKeyword"
          ! A.id "endKeyword"
          ! A.placeholder "Enter end keyword"
        H.br

        H.label ! A.for "transactionSourceId" $ "Transaction Source:"
        H.select ! A.name "transactionSourceId" $ do
          forM_ transactionSources $ \(Entity tsId ts) -> do
            H.option ! A.value (toValue $ fromSqlKey tsId) $ toHtml (transactionSourceName ts)
        H.br

        H.input ! A.type_ "submit" ! A.value "Add Configuration"

renderConfigurationPage ::
  Maybe FullSankeyConfig ->
  Map (Entity TransactionSource) [Entity Category] ->
  [Entity UploadConfiguration] ->
  [Entity TransactionSource] ->
  Html
renderConfigurationPage sankeyConfig transactions uploaderConfigs transactionSources =
  H.body $ do
    H.div ! A.class_ "container" $ do
      H.h1 "Configuration Page"

      newSourceComponent transactionSources False

      -- Upload Configurations Section
      H.div ! A.class_ "config-section" $ do
        renderUploadConfigurationsPage uploaderConfigs transactionSources

      -- Transaction Sources Section
      H.div ! A.class_ "config-section" $ do
        H.h2 "Manage Transaction Sources"
        H.form
          ! A.method "post"
          ! A.action "/add-transaction-source"
          $ do
            H.label ! A.for "newSource" $ "New Source Name:"
            H.input
              ! A.type_ "text"
              ! A.name "newSource"
              ! A.id "newSource"
            H.br
            H.input ! A.type_ "submit" ! A.value "Add Source"
        H.ul $ do
          forM_ transactionSources $ \(Entity sourceId source) -> do
            H.li $ do
              H.form
                ! A.method "post"
                ! A.action (toValue $ "/edit-transaction-source/" <> show (fromSqlKey sourceId))
                $ do
                  H.input
                    ! A.type_ "text"
                    ! A.name "sourceName"
                    ! A.value (toValue $ transactionSourceName source)
                  H.input ! A.type_ "submit" ! A.value "Rename"

      -- Categories Section
      H.div ! A.class_ "config-section" $ do
        H.h2 "Manage Categories"
        forM_ (Map.toList transactions) $ \(Entity sourceId source, categories) -> do
          H.h3 $ toHtml $ "Categories for " <> transactionSourceName source
          H.ul $ do
            forM_ categories $ \(Entity catId cat) -> do
              H.li $ do
                H.form
                  ! A.method "post"
                  ! A.action (toValue $ "/edit-category/" <> show (fromSqlKey catId))
                  $ do
                    H.input
                      ! A.type_ "text"
                      ! A.name "categoryName"
                      ! A.value (toValue $ categoryName cat)
                    H.input ! A.type_ "submit" ! A.value "Rename"
          H.form
            ! A.method "post"
            ! A.action (toValue $ "/add-category/" <> show (fromSqlKey sourceId))
            $ do
              H.label ! A.for "newCategory" $ "New Category Name:"
              H.input
                ! A.type_ "text"
                ! A.name "newCategory"
                ! A.id "newCategory"
              H.input ! A.type_ "submit" ! A.value "Add Category"
