{-# LANGUAGE OverloadedStrings #-}

module Main where

import Categorizer
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as B
import Data.Map
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Database
import HtmlGenerators.AllFilesPage
import HtmlGenerators.HomePage
import HtmlGenerators.HtmlGenerators
import HtmlGenerators.RefineSelectionPage
import Network.HTTP.Client (Request (redactHeaders))
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Network.Wai.Parse (FileInfo (..), tempFileBackEnd)
import Parsers
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Types
import Web.Scotty
import qualified Web.Scotty as Web

guessTransactions :: Text -> [Text]
guessTransactions txt =
  -- e.g. split on newlines, then filter or group:
  let linesAll = T.splitOn "\n" txt
   in linesAll -- Or do something more advanced

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev
  middleware $ staticPolicy (addBase "static")

  get "/" $ do
    content <- liftIO renderHomePage
    Web.html content

  get "/upload" $ do
    Web.html renderUploadPage

  post "/upload" $ do
    allFiles <- Web.Scotty.files
    case Prelude.lookup "pdfFile" allFiles of
      Nothing -> do
        Web.text "No file with field name 'pdfFile' was uploaded!"
      Just fileInfo -> do
        let uploadedBytes = fileContent fileInfo
        let originalName = decodeUtf8 $ fileName fileInfo

        let tempFilePath = "/tmp/" <> originalName
        liftIO $ B.writeFile (T.unpack tempFilePath) uploadedBytes

        extractedTextOrError <-
          liftIO $ try (extractTextFromPdf (T.unpack tempFilePath)) ::
            ActionM (Either SomeException Text)
        case extractedTextOrError of
          Left err -> do
            Web.text $ "Failed to parse the PDF: " <> TL.pack (show err)
          Right rawText -> do
            liftIO $ insertPdfRecord "transactions.db" originalName rawText
            newPdfId <- liftIO $ insertPdfRecord "transactions.db" originalName rawText
            redirect $ TL.fromStrict ("/adjust-transactions/" <> T.pack (show newPdfId))

  get "/adjust-transactions/:pdfId" $ do
    let dbPath = "transactions.db"
    pdfId <- pathParam "pdfId"
    (fileName, rawText) <- liftIO $ fetchPdfRecord dbPath pdfId
    let guessedSegments = guessTransactions rawText
    Web.html $ renderSliderPage pdfId fileName guessedSegments

  get "/transactions" $ do
    let dbPath = "transactions.db"
    filenames <- liftIO $ getAllFilenames dbPath
    Web.html $ renderAllFilesPage filenames

  get "/transactions/:filename" $ do
    let dbPath = "transactions.db"
    filename <- Web.Scotty.pathParam "filename"
    liftIO $ initializeDatabase dbPath
    transactions <- liftIO $ getAllTransactions dbPath filename
    Web.html $ renderTransactionsPage (T.pack filename) transactions

  post "/update-category" $ do
    let dbPath = "transactions.db"
    tId <- Web.Scotty.formParam "transactionId" :: ActionM T.Text
    newCat <- Web.Scotty.formParam "newCategory" :: ActionM T.Text
    fileArg <- Web.Scotty.formParam "filename" :: ActionM T.Text
    liftIO $ updateTransactionCategory dbPath (read $ T.unpack tId) newCat
    redirect $ TL.fromStrict ("/transactions/" <> fileArg)
