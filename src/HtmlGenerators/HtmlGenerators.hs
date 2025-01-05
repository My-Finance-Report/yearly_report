{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.HtmlGenerators
  ( renderTransactionsPage,
    renderUploadPage,
    renderSetupUploadPage,
    renderPdfResultPage,
  )
where

import Control.Monad (forM_)
import Data.List (sortBy)
import Data.Map hiding ((!))
import qualified Data.Map as Map hiding ((!))
import Data.Ord (comparing)
import Data.Text as T (Text, intercalate, pack)
import qualified Data.Text.Lazy as TL
import Data.Time
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Types

renderSetupUploadPage :: [TransactionSource] -> TL.Text
renderSetupUploadPage transactionSources =
  renderHtml $ docTypeHtml $ do
    H.head $ do
      H.title "Setup Upload Configuration"
      H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href "/style.css"
    H.body $ do
      H.h1 "Setup Upload Configuration"
      H.form
        ! A.method "post"
        ! A.action "/setup-upload"
        $ do
          -- Start Keyword
          H.label ! A.for "startKeyword" $ "Start Keyword:"
          H.input
            ! A.type_ "text"
            ! A.name "startKeyword"
            ! A.id "startKeyword"
            ! A.placeholder "Enter start keyword"
          H.br

          -- End Keyword
          H.label ! A.for "endKeyword" $ "End Keyword:"
          H.input
            ! A.type_ "text"
            ! A.name "endKeyword"
            ! A.id "endKeyword"
            ! A.placeholder "Enter end keyword"
          H.br

          -- Filename Pattern
          H.label ! A.for "filenamePattern" $ "Filename Pattern:"
          H.input
            ! A.type_ "text"
            ! A.name "filenamePattern"
            ! A.id "filenamePattern"
            ! A.placeholder "Enter filename pattern"
          H.br

          -- Transaction Source
          H.label ! A.for "transactionSourceId" $ "Transaction Source:"
          H.select
            ! A.name "transactionSourceId"
            ! A.id "transactionSourceId"
            $ do
              forM_ transactionSources $ \TransactionSource {sourceId, sourceName} -> do
                H.option
                  ! A.value (toValue sourceId)
                  $ toHtml sourceName
          H.br

          -- Submit Button
          H.input ! A.type_ "submit" ! A.value "Save Configuration"




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
