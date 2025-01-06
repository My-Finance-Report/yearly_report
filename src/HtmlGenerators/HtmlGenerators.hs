{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.HtmlGenerators
  ( renderTransactionsPage,
    renderUploadPage,
    renderPdfResultPage,
    renderEditSankeyConfigPage,
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
