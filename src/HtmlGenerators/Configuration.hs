{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.Configuration (renderConfigurationPage) where

import Control.Monad (forM_)
import Data.Map hiding ((!), (!?))
import qualified Data.Map as Map
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Types

renderEditSankeyConfigPage ::
  Maybe SankeyConfig ->
  Map TransactionSource [Category] ->
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
            Just config -> forM_ (inputs config) $ \(TransactionSource {sourceId}, category) -> do
              H.div $ do
                H.select
                  ! A.name "inputSourceId[]"
                  $ do
                    forM_ (Map.keys sourceCategories) $ \TransactionSource {sourceId = tsId, sourceName} -> do
                      H.option
                        ! A.value (toValue tsId)
                        !? (sourceId == tsId, A.selected "selected")
                        $ toHtml sourceName
                H.select
                  ! A.name "inputCategoryId[]"
                  $ do
                    let relevantCategories = Map.findWithDefault [] (TransactionSource sourceId "") sourceCategories
                    forM_ relevantCategories $ \Category {categoryId = catId, categoryName} -> do
                      H.option
                        ! A.value (toValue catId)
                        !? (catId == categoryId category, A.selected "selected")
                        $ toHtml categoryName
              H.br
            Nothing -> do
              H.div $ do
                H.select
                  ! A.name "inputSourceId[]"
                  $ do
                    forM_ (Map.keys sourceCategories) $ \TransactionSource {sourceId = tsId, sourceName} -> do
                      H.option
                        ! A.value (toValue tsId)
                        $ toHtml sourceName
                H.select
                  ! A.name "inputCategoryId[]"
                  $ do
                    forM_ (Prelude.concat $ Map.elems sourceCategories) $ \Category {categoryId = catId, categoryName} -> do
                      H.option
                        ! A.value (toValue catId)
                        $ toHtml categoryName
              H.br

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
            Just config -> do
              let (TransactionSource {sourceId = srcId}, linkCategory, TransactionSource {sourceId = tgtId}) = linkages config
              H.div $ do
                H.label "Source:"
                H.select
                  ! A.name "linkageSourceId"
                  $ do
                    forM_ (Map.keys sourceCategories) $ \TransactionSource {sourceId = tsId, sourceName} -> do
                      H.option
                        ! A.value (toValue tsId)
                        !? (srcId == tsId, A.selected "selected")
                        $ toHtml sourceName
                H.br
                H.label "Category:"
                H.select
                  ! A.name "linkageCategoryId"
                  $ do
                    let relevantCategories = Map.findWithDefault [] (TransactionSource srcId "") sourceCategories
                    forM_ relevantCategories $ \Category {categoryId = catId, categoryName} -> do
                      H.option
                        ! A.value (toValue catId)
                        !? (catId == categoryId linkCategory, A.selected "selected")
                        $ toHtml categoryName
                H.br
                H.label "Target:"
                H.select
                  ! A.name "linkageTargetId"
                  $ do
                    forM_ (Map.keys sourceCategories) $ \TransactionSource {sourceId = tsId, sourceName} -> do
                      H.option
                        ! A.value (toValue tsId)
                        !? (tgtId == tsId, A.selected "selected")
                        $ toHtml sourceName
            Nothing -> do
              H.div $ do
                H.label "Source:"
                H.select
                  ! A.name "linkageSourceId"
                  $ do
                    forM_ (Map.keys sourceCategories) $ \TransactionSource {sourceId = tsId, sourceName} -> do
                      H.option
                        ! A.value (toValue tsId)
                        $ toHtml sourceName
                H.br
                H.label "Category:"
                H.select
                  ! A.name "linkageCategoryId"
                  $ do
                    forM_ (Prelude.concat $ Map.elems sourceCategories) $ \Category {categoryId = catId, categoryName} -> do
                      H.option
                        ! A.value (toValue catId)
                        $ toHtml categoryName
                H.br
                H.label "Target:"
                H.select
                  ! A.name "linkageTargetId"
                  $ do
                    forM_ (Map.keys sourceCategories) $ \TransactionSource {sourceId = tsId, sourceName} -> do
                      H.option
                        ! A.value (toValue tsId)
                        $ toHtml sourceName

        H.br
        -- Submit Button
        H.input ! A.type_ "submit" ! A.value "Save Changes"

    -- JavaScript for Adding Inputs Dynamically
    H.script ! A.type_ "text/javascript" $ H.toHtml addInputScript

-- JavaScript to dynamically add new input rows
addInputScript :: Text
addInputScript =
  T.unlines
    [ "function addInputRow() {",
      "  const fieldset = document.querySelector('fieldset legend + div');",
      "  const newRow = fieldset.firstElementChild.cloneNode(true);",
      "  fieldset.appendChild(newRow);",
      "}"
    ]

-- TODO update this type
renderUploadConfigurationsPage :: [(Int, Text, Text, Text, TransactionSource)] -> [TransactionSource] -> Html
renderUploadConfigurationsPage configurations transactionSources =
  H.div $ do
    H.h1 "Upload Configurations"
    H.table ! A.class_ "upload-configurations" $ do
      H.tr $ do
        H.th "Filename Pattern"
        H.th "Start Keyword"
        H.th "End Keyword"
        H.th "Transaction Source"
        H.th "Actions"
      forM_ configurations $ \(configId, filenameRegex, startKeyword, endKeyword, TransactionSource {sourceId, sourceName}) -> do
        H.tr $ do
          H.form
            ! A.method "post"
            ! A.action (toValue $ "/edit-upload-config/" <> show configId)
            $ do
              H.td $ H.input ! A.type_ "text" ! A.name "filenameRegex" ! A.value (toValue filenameRegex)
              H.td $ H.input ! A.type_ "text" ! A.name "startKeyword" ! A.value (toValue startKeyword)
              H.td $ H.input ! A.type_ "text" ! A.name "endKeyword" ! A.value (toValue endKeyword)
              H.td
                $ H.select
                  ! A.name "transactionSourceId"
                $ do
                  forM_ transactionSources $ \TransactionSource {sourceId = tsId, sourceName} -> do
                    H.option
                      ! A.value (toValue tsId)
                      !? (tsId == sourceId, A.selected "selected")
                      $ toHtml sourceName
              H.td $ do
                H.input ! A.type_ "hidden" ! A.name "id" ! A.value (toValue configId)
                H.input ! A.type_ "submit" ! A.value "Save"
                H.form
                  ! A.method "post"
                  ! A.action (toValue $ "/delete-upload-config/" <> show configId)
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
          forM_ transactionSources $ \TransactionSource {sourceId, sourceName} -> do
            H.option ! A.value (toValue sourceId) $ toHtml sourceName
        H.br

        H.input ! A.type_ "submit" ! A.value "Add Configuration"

-- Render the configuration page
renderConfigurationPage ::
  Maybe SankeyConfig ->
  Map TransactionSource [Category] ->
  [(Int, Text, Text, Text, TransactionSource)] ->
  [TransactionSource] ->
  TL.Text
renderConfigurationPage sankeyConfig transactions uploaderConfigs transactionSources =
  renderHtml $ docTypeHtml $ do
    H.head $ do
      H.title "Configuration Page"
      H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href "/style.css"
    H.body $ do
      H.h1 "Configuration Page"

      -- General Settings Section
      H.div ! A.class_ "config-section" $ do
        H.h2 "Update General Settings"
        H.form
          ! A.method "post"
          ! A.action "/update-general-settings"
          $ do
            H.label ! A.for "setting1" $ "Setting 1:"
            H.input
              ! A.type_ "text"
              ! A.name "setting1"
              ! A.id "setting1"
            H.br
            H.label ! A.for "setting2" $ "Setting 2:"
            H.input
              ! A.type_ "number"
              ! A.name "setting2"
              ! A.id "setting2"
            H.br
            H.input ! A.type_ "submit" ! A.value "Save"

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
          forM_ transactionSources $ \TransactionSource {sourceId, sourceName} -> do
            H.li $ do
              H.form
                ! A.method "post"
                ! A.action (toValue $ "/edit-transaction-source/" <> show sourceId)
                $ do
                  H.input
                    ! A.type_ "text"
                    ! A.name "sourceName"
                    ! A.value (toValue sourceName)
                  H.input ! A.type_ "submit" ! A.value "Rename"
              H.form
                ! A.method "post"
                ! A.action (toValue $ "/delete-transaction-source/" <> show sourceId)
                $ H.input ! A.type_ "submit" ! A.value "Delete"

      -- Categories Section
      H.div ! A.class_ "config-section" $ do
        H.h2 "Manage Categories"
        forM_ (Map.toList transactions) $ \(TransactionSource {sourceId, sourceName}, categories) -> do
          H.h3 $ toHtml $ "Categories for " <> sourceName
          H.ul $ do
            forM_ categories $ \Category {categoryId, categoryName} -> do
              H.li $ do
                H.form
                  ! A.method "post"
                  ! A.action (toValue $ "/edit-category/" <> show categoryId)
                  $ do
                    H.input
                      ! A.type_ "text"
                      ! A.name "categoryName"
                      ! A.value (toValue categoryName)
                    H.input ! A.type_ "submit" ! A.value "Rename"
                H.form
                  ! A.method "post"
                  ! A.action (toValue $ "/delete-category/" <> show categoryId)
                  $ H.input ! A.type_ "submit" ! A.value "Delete"
          H.form
            ! A.method "post"
            ! A.action (toValue $ "/add-category/" <> show sourceId)
            $ do
              H.label ! A.for "newCategory" $ "New Category Name:"
              H.input
                ! A.type_ "text"
                ! A.name "newCategory"
                ! A.id "newCategory"
              H.input ! A.type_ "submit" ! A.value "Add Category"

