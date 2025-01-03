{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.HtmlGenerators (
     renderAllFilesPage
    , renderTransactionsPage
    , renderUploadPage
    , renderPdfResultPage
    , renderSliderPage
    , renderAdjustPage
    )
    where


import qualified Data.Map as Map hiding ((!))
import Data.Text as T
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


formatSankeyRow :: (Text, Text, Double) -> Text
formatSankeyRow (from, to, weight) =
  "['" <> from <> "', '" <> to <> "', " <> T.pack (show weight) <> "],\n"


renderAllFilesPage :: [T.Text] -> TL.Text
renderAllFilesPage filenames =
  renderHtmlT $ docTypeHtml $ do
    H.head $ do
      H.title "All Files"
    H.body $ do
      H.h1 "All Files"
      H.ul $ do
        mapM_ (filenameListItem "/transactions/") filenames

filenameListItem :: T.Text -> T.Text -> Html
filenameListItem baseUrl filename = H.li $ do
  let linkUrl = baseUrl <> filename
  H.a H.! A.href (toValue linkUrl) $ toHtml filename



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

renderSliderPage 
  :: Int        -- pdfId
  -> T.Text     -- filename
  -> [T.Text]   -- linesGuessed
  -> TL.Text
renderSliderPage pdfId filename linesGuessed =
  renderHtmlT $ docTypeHtml $ do
    H.head $ do
      H.title "Select Start/End with Sliders"
      

      H.link 
            ! A.rel "stylesheet"
            ! A.type_ "text/css"
            ! A.href "/style.css"

      H.script ! A.type_ "text/javascript" 
               ! A.src "/slider.js" 
               $ mempty

    H.body $ do
      -- Fixed Sliders Container
      H.div ! A.id "slidersContainer" $ do
        H.h1 "Select Transaction Boundaries"
        H.p $ toHtml ("File: " <> filename)

        -- "Start line" slider
        H.label ! A.for "startLine" $ "Start line: "
        H.input ! A.type_ "range" 
                ! A.id "startLine"
                ! A.name "startLine"
                ! A.min "0"
                ! A.max (toValue (Prelude.length linesGuessed - 1))
                ! A.value "0"
                ! A.oninput "updateRange()"

        H.br

        -- "End line" slider
        H.label ! A.for "endLine" $ "End line: "
        H.input ! A.type_ "range"
                ! A.id "endLine"
                ! A.name "endLine"
                ! A.min "0"
                ! A.max (toValue (Prelude.length linesGuessed - 1))
                ! A.value (toValue (Prelude.length linesGuessed - 1))
                ! A.oninput "updateRange()"

        H.form ! A.method "post" 
                ! A.action (toValue ("/confirm-boundaries/" <> show pdfId)) $ do
            -- Hidden inputs to store chosen lines
            H.input ! A.type_ "hidden" ! A.name "finalStart" ! A.id "finalStartId"
            H.input ! A.type_ "hidden" ! A.name "finalEnd"   ! A.id "finalEndId"
            H.br
            H.input ! A.type_ "submit" ! A.value "Submit"

      H.br
      H.br

      -- Container for displaying lines with top padding to prevent overlap
      H.pre ! A.id "linesContainer"
            ! A.class_ "linesContainer" $ do
        forM_ (Prelude.zip [0..] linesGuessed) $ \(idx, txt) -> do
            H.span ! A.id (toValue $ "line-" <> show idx) $ toHtml txt
            -- Insert a newline to preserve line structure:
            toHtml ("\n" :: String)

      -- A hidden form for submission



