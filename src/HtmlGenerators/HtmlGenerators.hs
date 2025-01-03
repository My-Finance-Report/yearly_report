{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.HtmlGenerators (
    renderTransactionsPage
    , renderUploadPage
    , renderPdfResultPage
    , renderAdjustPage
    )
    where


import qualified Data.Map as Map hiding ((!))
import Data.Text as T ( Text, intercalate, pack )
import qualified Data.Text.Lazy as TL
import Data.List (sortBy)
import Data.Ord (comparing)
import Types
import Data.Map hiding ((!))
import Data.Time
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Control.Monad (forM_)


renderUploadPage :: TL.Text
renderUploadPage = renderHtmlT $ H.docTypeHtml $ do
  H.head $ do
    H.title "Upload PDF"
  H.body $ do
    H.h1 "Upload a PDF"
    H.form H.! A.method "post" H.! A.action "/upload" 
           H.! A.enctype "multipart/form-data" $ do
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
      let desc    = description (transaction tx)
          dateTxt = T.pack $ show (transactionDate (transaction tx))
          amt     = amount (transaction tx)
          cat     = category tx

      H.tr $ do
        H.td (toHtml tid)
        H.td (toHtml desc)
        H.td (toHtml dateTxt)
        H.td (toHtml amt)
        H.td (toHtml cat)
        H.td $ H.form H.! A.method "post" H.! A.action "/update-category" $ do
            H.input H.! A.type_ "hidden" H.! A.name "transactionId" H.! A.value (toValue tid)
            H.input H.! A.type_ "hidden" H.! A.name "filename"      H.! A.value (toValue filename)
            H.input H.! A.type_ "text"   H.! A.name "newCategory"
            H.input H.! A.type_ "submit" H.! A.value "Update"

renderHtmlT :: Html -> TL.Text
renderHtmlT = renderHtml


renderAdjustPage :: Int -> Text -> [Text] -> TL.Text
renderAdjustPage pdfId filename guessed = 
  renderHtmlT $ H.docTypeHtml $ do
    H.head $ do
      H.title "Adjust Transactions"
    H.body $ do
      H.h1 "Adjust Guessed Transactions"
      H.p $ toHtml $ "File: " <> filename

      -- A form that will POST the final lines to /confirm-transactions
      H.form ! A.method "post" ! A.action (H.toValue $ "/confirm-transactions/" <> show pdfId) $ do
        H.textarea ! A.rows "20" ! A.cols "80" ! A.name "finalText" $
          toHtml (T.intercalate "\n" guessed)
        H.br
        H.input ! A.type_ "submit" ! A.value "Submit"

