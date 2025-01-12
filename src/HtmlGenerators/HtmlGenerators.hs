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
import Database.Models
import Database.Persist.Postgresql (BackendKey (SqlBackendKey), Entity (entityKey, entityVal), fromSqlKey)
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

renderTransactionsPage :: Entity UploadedPdf -> [CategorizedTransaction] -> Html
renderTransactionsPage file txs =
  body $ do
    H.h1 $ toHtml $ "Transactions for " <> uploadedPdfFilename (entityVal file)
    H.table $ do
      H.tr $ do
        H.th "Id"
        H.th "Description"
        H.th "Date"
        H.th "Amount"
        H.th "Category"
        H.th "Actions"
      mapM_ (renderEditableTransactionRow file) txs

renderEditableTransactionRow :: Entity UploadedPdf -> CategorizedTransaction -> Html
renderEditableTransactionRow file tx = do
  let Transaction {transactionDescription, transactionDateOfTransaction, transactionAmount} = entityVal $ transaction tx
      Category {categoryName} = entityVal $ category tx
      catId = fromSqlKey $ entityKey $ category tx
      txId = case transactionId tx of
        Just (TransactionKey (SqlBackendKey key)) -> T.pack (show key)
        Nothing -> "Unknown"
  H.form
    ! A.method "post"
    ! A.action (toValue $ "/update-transaction/" <> txId)
    $ do
      H.tr $ do
        H.td $ toHtml txId
        H.td $
          H.input
            ! A.type_ "text"
            ! A.name "description"
            ! A.class_ "full-width"
            ! A.value (toValue transactionDescription)
        H.td $
          H.input
            ! A.type_ "date"
            ! A.name "transactionDate"
            ! A.class_ "full-width"
            ! A.value (toValue $ formatMonthYear transactionDateOfTransaction)
        H.td $
          H.input
            ! A.type_ "number"
            ! A.step "0.01"
            ! A.name "amount"
            ! A.class_ "full-width"
            ! A.value (toValue $ show transactionAmount)
        H.td
          $ H.select
            ! A.name "category"
            ! A.class_ "full-width"
          $ do
            -- TODO these need to be the available categorys
            H.option
              ! A.value (toValue catId)
              ! A.selected "selected"
              $ toHtml categoryName
            H.option ! A.value "Other" $ "Other"
        H.input
          ! A.type_ "hidden"
          ! A.name "fileId"
          ! A.value (toValue $ fromSqlKey $ entityKey file)
        H.td $
          H.input
            ! A.type_ "submit"
            ! A.class_ "full-width"
            ! A.value "Save"

renderHtmlT :: Html -> TL.Text
renderHtmlT = renderHtml

formatMonthYear :: UTCTime -> T.Text
formatMonthYear utcTime = T.pack (formatTime defaultTimeLocale "%Y-%m-%d" utcTime)