{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.HtmlGenerators
  ( renderTransactionsPage,
    renderUploadPage,
    renderPdfResultPage,
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
import Models
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Types (CategorizedTransaction (transaction, transactionId), category)

renderUploadPage :: TL.Text
renderUploadPage = renderHtmlT $ H.docTypeHtml $ do
  H.head $ do
    H.title "Upload PDF"
    H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "/static/css/navbar.css"
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
      H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "/static/css/navbar.css"
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
      H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "/static/css/navbar.css"
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
          H.th "Actions"
        mapM_ (renderEditableTransactionRow filename) txs

-- TODO this should be removed
renderEditableTransactionRow :: T.Text -> CategorizedTransaction -> Html
renderEditableTransactionRow filename tx = do
  let Transaction {transactionDescription, transactionDateOfTransaction, transactionAmount} = transaction tx
      Category {categoryName} = category tx
      txId = transactionId tx
  H.tr $ do
    -- Transaction ID
    H.td $ toHtml $ show txId
    -- Description (editable)
    H.td
      $ H.form
        ! A.method "post"
        ! A.action (toValue $ "/update-transaction/" <> T.pack (show txId))
      $ do
        H.input
          ! A.type_ "text"
          ! A.name "description"
          ! A.value (toValue transactionDescription)
    -- Date (editable)
    H.td $
      H.input
        ! A.type_ "date"
        ! A.name "transactionDate"
        ! A.value (toValue $ show transactionDateOfTransaction)
    -- Amount (editable)
    H.td $
      H.input
        ! A.type_ "number"
        ! A.step "0.01"
        ! A.name "amount"
        ! A.value (toValue $ show transactionAmount)
    -- Category (editable dropdown)
    H.td $
      H.select ! A.name "category" $ do
        H.option
          ! A.value (toValue categoryName)
          ! A.selected "selected"
          $ toHtml categoryName
        H.option ! A.value "Other" $ "Other" -- Example, expand this as needed
        -- Override (editable)
    H.td $
      H.input
        ! A.type_ "text"
        ! A.name "override"
    -- Submit button
    H.td $
      H.input
        ! A.type_ "submit"
        ! A.value "Save"

renderHtmlT :: Html -> TL.Text
renderHtmlT = renderHtml
