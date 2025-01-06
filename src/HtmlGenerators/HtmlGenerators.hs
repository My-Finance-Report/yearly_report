{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.HtmlGenerators
  ( renderTransactionsPage,
    renderUploadPage,
    renderPdfResultPage,
    renderEditSankeyConfigPage,
    renderUploadConfigurationsPage,
  )
where

import Control.Monad (forM_)
import Data.List (sortBy)
import Data.Map hiding ((!), (!?))
import qualified Data.Map as Map hiding ((!), (!?))
import Data.Ord (comparing)
import Data.Text as T (Text, intercalate, pack, unlines)
import qualified Data.Text.Lazy as TL
import Data.Time
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Types

renderUploadPage :: TL.Text
renderUploadPage = renderHtmlT $ H.docTypeHtml $ do
  H.head $ do
    H.title "Upload PDF"
  H.body $ do
    H.h1 "Upload a PDF"
    H.form
      H.! A.method "post"
      H.! A.action "/upload"
      H.! A.enctype "multipart/form-data"
      $ do
        H.label "Choose PDF to upload:"
        H.br
        H.input H.! A.type_ "file" H.! A.name "pdfFile"
        H.br
        H.input H.! A.type_ "submit" H.! A.value "Upload"

renderPdfResultPage :: Text -> Text -> TL.Text
renderPdfResultPage filename rawText =
  renderHtml $ H.docTypeHtml $ do
    H.head $ do
      H.title "PDF Upload Result"
    H.body $ do
      H.h1 "PDF Uploaded Successfully!"
      H.p $ do
        "Filename: "
        H.b (toHtml filename)
      H.h2 "Extracted Text"
      H.pre (toHtml rawText)

renderTransactionsPage :: T.Text -> [CategorizedTransaction] -> TL.Text
renderTransactionsPage filename txs =
  renderHtmlT $ docTypeHtml $ do
    H.head $ do
      H.title "Transactions"
      H.style "table, th, td { border: 1px solid black; border-collapse: collapse; padding: 8px }"
    H.body $ do
      H.h1 $ toHtml $ "Transactions for " <> filename
      H.table $ do
        H.tr $ do
          H.th "Id"
          H.th "Description"
          H.th "Date"
          H.th "Amount"
          H.th "Category"
          H.th "Override"
        mapM_ (renderTransactionRow filename) txs

renderTransactionRow :: T.Text -> CategorizedTransaction -> Html
renderTransactionRow filename tx =
  case transactionId tx of
    Nothing -> error "No transactionId in record—cannot render row!"
    Just tid -> do
      let desc = description (transaction tx)
          dateTxt = T.pack $ show (transactionDate (transaction tx))
          amt = amount (transaction tx)
          cat = categoryName (category tx)

      H.tr $ do
        H.td (toHtml tid)
        H.td (toHtml desc)
        H.td (toHtml dateTxt)
        H.td (toHtml amt)
        H.td (toHtml cat)
        H.td $ H.form H.! A.method "post" H.! A.action "/update-category" $ do
          H.input H.! A.type_ "hidden" H.! A.name "transactionId" H.! A.value (toValue tid)
          H.input H.! A.type_ "hidden" H.! A.name "filename" H.! A.value (toValue filename)
          H.input H.! A.type_ "text" H.! A.name "newCategory"
          H.input H.! A.type_ "submit" H.! A.value "Update"

renderHtmlT :: Html -> TL.Text
renderHtmlT = renderHtml

renderEditSankeyConfigPage ::
  Maybe SankeyConfig ->
  Map.Map TransactionSource [Category] ->
  TL.Text
renderEditSankeyConfigPage maybeConfig sourceCategories =
  renderHtml $ docTypeHtml $ do
    H.head $ do
      H.title "Edit or Create Sankey Configuration"
      H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href "/style.css"
    H.body $ do
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
                      forM_ (concat $ Map.elems sourceCategories) $ \Category {categoryId = catId, categoryName} -> do
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
                      forM_ (concat $ Map.elems sourceCategories) $ \Category {categoryId = catId, categoryName} -> do
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

renderUploadConfigurationsPage :: [(Int, Text, Text, Text, TransactionSource)] -> [TransactionSource] -> TL.Text
renderUploadConfigurationsPage configurations transactionSources =
  renderHtml $ docTypeHtml $ do
    H.head $ do
      H.title "Manage Upload Configurations"
      H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href "/style.css"
    H.body $ do
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
