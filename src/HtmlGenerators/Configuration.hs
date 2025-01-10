{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.Configuration (renderConfigurationPage) where

import Control.Monad (forM_)
import Data.Map hiding ((!), (!?))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Database.Persist
import Database.Persist.Postgresql (fromSqlKey)
import Models
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Types

renderEditSankeyConfigPage ::
  Maybe FullSankeyConfig ->
  Map (Entity TransactionSource) [Entity Category] ->
  Html
renderEditSankeyConfigPage maybeConfig sourceCategories =
  H.div $ do
    H.h1 "Edit or Create Sankey Configuration"

    H.form
      ! A.method "post"
      ! A.action "/update-sankey-config"
      $ do
        -- Configuration Name
        H.label ! A.for "configName" $ "Configuration Name:"
        H.input
          ! A.type_ "text"
          ! A.name "configName"
          ! A.id "configName"
          ! A.value (toValue $ maybe "" configName maybeConfig)
        H.br

        -- Inputs Section
        H.fieldset $ do
          H.legend "Inputs"
          case maybeConfig of
            Just FullSankeyConfig {inputs} ->
              forM_ inputs $ \(source, category) ->
                renderInputRow sourceCategories source (entityKey category) (categoryName $ entityVal category)
            Nothing -> renderEmptyInputRow sourceCategories
          -- Add New Input Button
          H.button
            ! A.type_ "button"
            ! A.onclick "addInputRow()"
            $ "Add New Input"

        H.br

        -- Linkages Section
        H.fieldset $ do
          H.legend "Linkages"
          case maybeConfig of
            Just FullSankeyConfig {linkages = (source, category, target)} ->
              renderLinkageRow sourceCategories source (entityKey category) (entityKey target)
            Nothing -> renderEmptyLinkageRow sourceCategories

        H.br
        -- Submit Button
        H.input ! A.type_ "submit" ! A.value "Save Changes"

    -- JavaScript for Adding Inputs Dynamically
    H.script ! A.type_ "text/javascript" $ H.toHtml addInputScript

renderInputRow ::
  Map (Entity TransactionSource) [Entity Category] ->
  Entity TransactionSource ->
  Key Category ->
  Text ->
  Html
renderInputRow sourceCategories ts catId catName = do
  H.div $ do
    -- Source Dropdown
    H.select ! A.name "inputSourceId[]" $ do
      forM_ (Map.keys sourceCategories) $ \(Entity sourceId source) -> do
        H.option
          ! A.value (toValue $ fromSqlKey sourceId)
          !? (sourceId == entityKey ts, A.selected "selected")
          $ toHtml
          $ transactionSourceName source
    -- Category Dropdown
    H.select ! A.name "inputCategoryId[]" $ do
      let relevantCategories = Map.findWithDefault [] ts sourceCategories
      forM_ relevantCategories $ \(Entity categoryId category) -> do
        H.option
          ! A.value (toValue $ fromSqlKey categoryId)
          !? (catId == categoryId, A.selected "selected")
          $ toHtml (categoryName category)

-- Render empty input row
renderEmptyInputRow ::
  Map (Entity TransactionSource) [Entity Category] ->
  Html
renderEmptyInputRow sourceCategories = do
  H.div $ do
    -- Source Dropdown
    H.select ! A.name "inputSourceId[]" $ do
      forM_ (Map.keys sourceCategories) $ \(Entity sourceId source) -> do
        H.option
          ! A.value (toValue $ fromSqlKey sourceId)
          $ toHtml
          $ transactionSourceName source
    -- Category Dropdown
    H.select ! A.name "inputCategoryId[]" $ do
      forM_ (Prelude.concat $ Map.elems sourceCategories) $ \(Entity catId category) -> do
        H.option
          ! A.value (toValue $ fromSqlKey catId)
          $ toHtml
          $ categoryName category

renderLinkageRow ::
  Map (Entity TransactionSource) [Entity Category] ->
  Entity TransactionSource ->
  Key Category ->
  Key TransactionSource ->
  Html
renderLinkageRow sourceCategories src catId tgtId = do
  H.div $ do
    -- Source Dropdown
    H.select ! A.name "linkageSourceId" $ do
      forM_ (Map.keys sourceCategories) $ \(Entity sourceId source) -> do
        H.option
          ! A.value (toValue $ fromSqlKey sourceId)
          !? (sourceId == entityKey src, A.selected "selected")
          $ toHtml
          $ transactionSourceName source
    -- Category Dropdown
    H.select ! A.name "linkageCategoryId" $ do
      let relevantCategories = Map.findWithDefault [] src sourceCategories
      forM_ relevantCategories $ \(Entity categoryId category) -> do
        H.option
          ! A.value (toValue $ fromSqlKey categoryId)
          !? (catId == categoryId, A.selected "selected")
          $ toHtml
          $ categoryName category
    -- Target Dropdown
    H.select ! A.name "linkageTargetId" $ do
      forM_ (Map.keys sourceCategories) $ \(Entity sourceId source) -> do
        H.option
          ! A.value (toValue $ fromSqlKey sourceId)
          !? (sourceId == tgtId, A.selected "selected")
          $ toHtml
          $ transactionSourceName source

-- Render empty linkage row
renderEmptyLinkageRow ::
  Map (Entity TransactionSource) [Entity Category] ->
  Html
renderEmptyLinkageRow sourceCategories = do
  H.div $ do
    -- Source Dropdown
    H.select ! A.name "linkageSourceId" $ do
      forM_ (Map.keys sourceCategories) $ \(Entity sourceId source) -> do
        H.option
          ! A.value (toValue $ fromSqlKey sourceId)
          $ toHtml
          $ transactionSourceName source
    -- Category Dropdown
    H.select ! A.name "linkageCategoryId" $ do
      forM_ (Prelude.concat $ Map.elems sourceCategories) $ \(Entity catId category) -> do
        H.option
          ! A.value (toValue $ fromSqlKey catId)
          $ toHtml
          $ categoryName category
    -- Target Dropdown
    H.select ! A.name "linkageTargetId" $ do
      forM_ (Map.keys sourceCategories) $ \(Entity sourceId source) -> do
        H.option
          ! A.value (toValue $ fromSqlKey sourceId)
          $ toHtml
          $ transactionSourceName source

addInputScript :: Text
addInputScript =
  T.unlines
    [ "function addInputRow() {",
      "  const fieldset = document.querySelector('fieldset legend + div');",
      "  const newRow = fieldset.firstElementChild.cloneNode(true);",
      "  fieldset.appendChild(newRow);",
      "}"
    ]

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
            ! A.method "post" -- Use POST for compatibility
            ! A.action (toValue $ "/delete-upload-config/" <> show (fromSqlKey configId))
            $ H.td
            $ H.input ! A.type_ "submit" ! A.value "Delete"

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
      H.h1 "Configuration Page"

      -- Upload Configurations Section
      H.div ! A.class_ "config-section" $ do
        renderUploadConfigurationsPage uploaderConfigs transactionSources

      -- Sankey Configuration Section
      H.div ! A.class_ "config-section" $ do
        renderEditSankeyConfigPage sankeyConfig transactions

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
