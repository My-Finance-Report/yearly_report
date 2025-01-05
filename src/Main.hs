{-# LANGUAGE OverloadedStrings #-}

module Main where

import Categorizer
import Control.Concurrent.Async (async)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as B
import Data.IORef (modifyIORef, newIORef, readIORef)
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

main :: IO ()
main = do
  let dbPath = "transactions.db"

  activeJobs <- newIORef 0
  initializeDatabase dbPath
  scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "static")

    get "/" $ do
      activeJobs <- liftIO $ readIORef activeJobs
      let banner = if activeJobs > 0 then Just "Job Running" else Nothing
      liftIO $ print banner
      content <- liftIO $ renderHomePage banner
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
      transactionSources <- liftIO $ getAllTransactionSources dbPath
      (fileName, rawText) <- liftIO $ fetchPdfRecord dbPath pdfId
      let segments = T.splitOn "\n" rawText

      Web.html $ renderSliderPage pdfId fileName segments transactionSources

    post "/confirm-boundaries/:pdfId" $ do
      pdfId <- Web.Scotty.pathParam "pdfId"
      finalStart <- Web.Scotty.formParam "finalStart" :: ActionM T.Text
      finalEnd <- Web.Scotty.formParam "finalEnd" :: ActionM T.Text
      txnSourceId <- Web.Scotty.formParam "transactionSourceId"

      let dbPath = "transactions.db"

      (fileName, rawText) <- liftIO $ fetchPdfRecord dbPath pdfId
      let allLines = T.lines rawText
      let startIdx = read $ T.unpack finalStart
      let endIdx = read $ T.unpack finalEnd
      let filenameRegex = read $ T.unpack filenameRegex

      let startKeyword = allLines !! startIdx
      let endKeyword = allLines !! endIdx

      theTransactionSource <- liftIO $ getTransactionSource dbPath txnSourceId

      activeJobs <- liftIO $ newIORef 0
      liftIO $ do
        modifyIORef activeJobs (+ 1)
        persistUploadConfiguration dbPath pdfId startKeyword endKeyword txnSourceId filenameRegex
        let config = UploadConfiguration {transactionSourceId = txnSourceId, filenameRegex = filenameRegex, endKeyword = endKeyword, startKeyword = startKeyword} {startKeyword = startKeyword, endKeyword = endKeyword, filenameRegex = filenameRegex, transactionSourceId = txnSourceId}
        _ <- Control.Concurrent.Async.async $ do
          processPdfFile dbPath pdfId config
          modifyIORef activeJobs (subtract 1)
        return ()

      redirect "/"

    get "/transactions" $ do
      let dbPath = "transactions.db"
      filenames <- liftIO $ getAllFilenames dbPath
      Web.html $ renderAllFilesPage filenames

    get "/transactions/:filename" $ do
      let dbPath = "transactions.db"
      filename <- Web.Scotty.pathParam "filename"
      liftIO $ initializeDatabase dbPath
      transactions <- liftIO $ getTransactionsByFilename dbPath filename
      Web.html $ renderTransactionsPage (T.pack filename) transactions

    post "/update-category" $ do
      let dbPath = "transactions.db"
      tId <- Web.Scotty.formParam "transactionId" :: ActionM T.Text
      newCat <- Web.Scotty.formParam "newCategory" :: ActionM T.Text
      fileArg <- Web.Scotty.formParam "filename" :: ActionM T.Text
      liftIO $ updateTransactionCategory dbPath (read $ T.unpack tId) newCat
      redirect $ TL.fromStrict ("/transactions/" <> fileArg)
